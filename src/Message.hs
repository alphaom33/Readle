module Message where
import Data.Text (Text)

data Message = NextPage | LastPage | NewSearch | EnterSite String | Error String deriving Show
