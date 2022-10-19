module User
  ( userRouter,
  )
where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import GHC.Generics
import Postgres (withConn)
import Types (Config, db)
import Web.Scotty
import Data.Text

data User = User {id :: Int, name :: Text} deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

instance FromRow User

instance ToRow User

mockUsers :: [User]
mockUsers =
  [ User 1 "mihnea",
    User 2 "sebi",
    User 5 "ion"
  ]

userRouter :: Config -> ScottyM ()
userRouter config = do
  get "/users" $ json mockUsers
  readUser config
  createUser

readUser :: Config -> ScottyM ()
readUser config = get "/user/:id" $ do
  userId <- param "id"
  user <- getUserById config userId
  json user

getUserById :: MonadIO m => Config -> Int -> m (Maybe User)
getUserById config userId = do
  res <- withConn (db config) $ \conn -> query conn "Select id,name from users where id = ?" (Only userId)
  return $ case res of
    [(id, name)] -> Just $ User id name
    _ -> Nothing

createUser :: ScottyM ()
createUser = post "/user" $ text "not implemented"
