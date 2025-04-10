module Message where
import Data.Text (Text)

data Message = NewSearch | EnterSite String | Error String deriving Show
