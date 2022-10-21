module Types
  ( Config (..),
    Response (..),
  )
where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Text (Text)

newtype Config = Config {db :: Pool Connection}

data Response = Response {success :: Bool, message :: Text} deriving (Generic)

instance ToJSON Response
