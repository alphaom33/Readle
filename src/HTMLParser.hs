{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module HTMLParser where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Cont (MonadIO(liftIO))
import Debug.Trace (trace)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \case
  []                 -> ([], Left "end of stream")
  (c:cs) | f c       -> (cs, Right c)
         | otherwise -> (cs, Left "did not satisfy")

try :: Parser a -> Parser a
try (P f) = P $ \stream0 -> case f stream0 of
  (_      , Left err) -> (stream0, Left err)
  (stream1, Right a ) -> (stream1, Right a )

orElse :: Parser a -> Parser a -> Parser a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left err) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)

type Error = String
newtype Parser a = P { parse :: String -> (String, Either Error a) }

instance Functor Parser where
  fmap f (P st) = P $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative Parser where
  pure a = P (, Right a)
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

instance Alternative Parser where
  empty = P (, Left "empty")
  (<|>) = orElse

  many = manyParser
  some = someParser

-- | 0 or more
manyParser :: Parser a -> Parser [a]
manyParser (P f) = P go where
  go stream = case f stream of
    (_      , Left err) -> (stream, Right [])  -- throws away the error
    (stream', Right a ) -> case go stream' of
      (streamFin, Left err) -> (streamFin, Left err)
      (streamFin, Right as) -> (streamFin, Right (a : as))

-- | 1 or more
someParser :: Parser a -> Parser [a]
someParser (P f) = P $ \stream -> case f stream of
  (stream', Left err) -> (stream', Left err)
  (stream', Right a ) ->
    let (P fmany) = manyParser (P f)
    in case fmany stream' of
      (stream'', Left err) -> (stream'', Left err)
      (stream'', Right as) -> (stream'', Right (a:as))

data Tag = Tag {
    name :: String,
    children :: [Tag],
    text :: String
} deriving Show

char :: Char -> Parser Char
char c = satisfy (== c)

dropFirstAndLast _ a _ = a
dropFirstsAndLast _ _ a _ = a

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'

alpha :: Parser Char
alpha = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz")

numeric :: Parser Char
numeric = satisfy (`elem` "0123456789")

alphanumeric :: Parser Char
alphanumeric = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz0123456789")

consume :: Parser Char
consume = satisfy $ const True

identifier :: Parser String
identifier = (:) <$> alpha <*> manyParser alphanumeric

matchIdentifier :: String -> Parser String
matchIdentifier = foldr (\ c -> (<*>) ((:) <$> char c)) (pure [])

startTag :: Parser String
startTag = dropFirstAndLast <$> char '<' <*> identifier <*> char '>'

endTag :: String -> Parser String
endTag identifier = dropFirstsAndLast <$> char '<' <*> char '/' <*> matchIdentifier identifier <*> char '>'

finishTag :: String -> (String, String, [Tag])
finishTag toFinish = do
    let (rest, Right text) = parse (manyParser $ satisfy (/= '<')) toFinish
    let (rest', name) = parse (try startTag) rest
    case name of
        Right e -> do
            let (rest'', child) = parseTag e rest'
            let (rest''', texts, childs) = finishTag rest''
            (rest''', text ++ texts, child:childs)
        Left _ -> (rest', text, [])

parseTag :: String -> String -> (String, Tag)
parseTag name string = do
    let (rest, text, children) = finishTag string
    let (rest', result) = parse (endTag name) rest
    case result of
        Right _ -> (rest', Tag {name=name, children=children, text=text})
        Left e -> (rest', Tag {name="err", children=[], text=e})

parseString :: String -> (String, Tag)
parseString string = do
    let (rest, Right name) = parse startTag string
    parseTag name rest
