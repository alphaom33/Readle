module HTMLParser where

import Control.Applicative
import Control.Monad (join)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \stream -> case stream of
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
  pure a = P (\stream -> (stream, Right a))
  P ff <*> P xx = P $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

instance Alternative Parser where
  empty = P $ \stream -> (stream, Left "empty")
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

newtype Tag = String String

char :: Char -> Parser Char
char c = satisfy (== c)

notQuote c = satisfy (/= '"')

dropFirstAndLast _ a _ = a
dropFirstsAndLast _ _ a _ = a

parens :: Parser a -> Parser a
parens parseA = dropFirstAndLast <$> char '(' <*> parseA <*> char ')'

alpha = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz")
alphanumeric = satisfy (`elem` "abcdefghijklmnopqrstuvqwxyz0123456789")
consume = satisfy (\_ -> True)

identifier = (:) <$> alpha <*> manyParser alphanumeric

matchIdentifier [] = pure []
matchIdentifier (c:cs) = (:) <$> char c <*> matchIdentifier cs

startTag = dropFirstAndLast <$> char '<' <*> identifier <*> char '>'
endTag identifier = dropFirstsAndLast <$> char '<' <*> char '/' <*> matchIdentifier identifier <*> char '>'

parseString = parse tag