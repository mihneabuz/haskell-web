module User.User
  ( userRouter,
  )
where

import Control.Monad.IO.Class
import Data.Aeson (decode)
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Postgres (withConn)
import Types (Config, db)
import User.Types
import Utils
import Web.Scotty

userRouter :: Config -> ScottyM ()
userRouter config = do
  mapM_ (\r -> r config) [readUsers, readUser, createUser]

{--                       Routes                         --}

readUsers :: Config -> ScottyM ()
readUsers config = get "/users" $ do
  users <- getAllUsers config
  json users

readUser :: Config -> ScottyM ()
readUser config = get "/user/:id" $ do
  userId <- param "id"
  user <- getUserById config userId
  json user

createUser :: Config -> ScottyM ()
createUser config = post "/user" $ do
  payload <- decode <$> body
  case payload of
    Nothing -> text "bad request"
    Just (CreateUserPayload n) -> do
      res <- insertUser config (User 5 n)
      json res

{--                 Querying Database                    --}

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

insertUser :: MonadIO m => Config -> User -> m Int64
insertUser config = userMutation config qry
  where
    qry = "insert into users values (?, ?)"

userQuery :: (MonadIO m, ToRow a) => Config -> Query -> a -> m [User]
userQuery config qry arg = withConn (db config) $ \con -> query con qry arg

userMutation :: (MonadIO m, ToRow a) => Config -> Query -> a -> m Int64
userMutation config qry arg = withConn (db config) $ \con -> execute con qry arg
