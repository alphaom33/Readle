module Message where
import Data.Text (Text)
import Item (Item)

data Message = NewData [Item] | NextPage | LastPage | NewSearch | EnterSite String | Error String deriving Show
