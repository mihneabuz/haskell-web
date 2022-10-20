module User.Types
  ( User (..),
    CreateUserPayload (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow, ToRow)

data User = User {id :: Int, name :: Text} deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

instance FromRow User

instance ToRow User

data CreateUserPayload = CreateUserPayload {name :: Text} deriving (Generic)

instance FromJSON CreateUserPayload
