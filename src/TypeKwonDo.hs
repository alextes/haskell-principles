module TypeKwonDo where

import           Data.List  (find)
import           Data.Maybe (fromJust)

data Code
  = GreenWheels
  | Uber
  deriving (Eq)

data Provider = Provider
  { code  :: Code
  , title :: String
  }

providers =
  [ Provider {code = GreenWheels, title = "Yipee"}
  , Provider {code = Uber, title = "Yahaaa"}
  ]

getTitle :: Code -> Maybe String
getTitle c = find ((==) c . code) providers >>= \p -> return (title p)

