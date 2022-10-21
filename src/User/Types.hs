module User.Types
  ( UserDb (..),
    UserEntity (..),
    toEntity,
    CreateUserPayload (..),
    AuthPayload (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import Database.PostgreSQL.Simple (FromRow, ToRow)

data UserDb = UserDb {id :: Int, username :: Text, mail :: Text, password :: Text} deriving (Show, Generic)

instance ToJSON UserDb

instance FromJSON UserDb

instance FromRow UserDb

instance ToRow UserDb

data UserEntity = UserEntity {username :: Text, mail :: Text} deriving (Show, Generic)

instance ToJSON UserEntity

instance FromJSON UserEntity

instance FromRow UserEntity

instance ToRow UserEntity

toEntity :: UserDb -> UserEntity
toEntity (UserDb _ n e _) = UserEntity n e

data CreateUserPayload = CreateUserPayload {username :: Text, mail :: Text, password :: Text} deriving (Show, Generic)

instance FromJSON CreateUserPayload

data AuthPayload = AuthPayload {username :: Text, password :: Text} deriving (Show, Generic)

instance FromJSON AuthPayload
