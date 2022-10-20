module User
  ( userRouter,
  )
where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics
import Postgres (withConn)
import Types (Config, db)
import Utils
import Web.Scotty

data User = User {id :: Int, name :: Data.Text.Text} deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User

userRouter :: Config -> ScottyM ()
userRouter config = do
  readUsers config
  readUser config
  createUser

readUsers :: Config -> ScottyM ()
readUsers config = get "/users" $ do
  users <- getAllUsers config
  json users

readUser :: Config -> ScottyM ()
readUser config = get "/user/:id" $ do
  userId <- param "id"
  user <- getUserById config userId
  json user

createUser :: ScottyM ()
createUser = post "/user" $ text "not implemented"

getUserById :: MonadIO m => Config -> Int -> m (Maybe User)
getUserById config searchId = maybeHead <$> userQuery config qry arg
  where
    qry = "Select id,name from users where id = ?"
    arg = Only searchId

getAllUsers :: MonadIO m => Config -> m [User]
getAllUsers config = userQuery config qry arg
  where
    qry = "Select id,name from users"
    arg = ()

userQuery :: (MonadIO m, ToRow a) => Config -> Query -> a -> m [User]
userQuery config qry arg = withConn (db config) $ \con -> query con qry arg
