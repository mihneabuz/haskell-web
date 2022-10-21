module User.User
  ( userRouter,
  )
where

import Control.Monad.IO.Class
import Data.Aeson (decode)
import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Postgres (withConn, withConnUnchecked)
import Types (Config, db)
import User.Types
import Utils
import Web.Scotty

userRouter :: Config -> ScottyM ()
userRouter config = do
  mapM_ (\r -> r config) [readUsers, readUser, createUser, authUser]

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
    Nothing -> json $ resBad "bad request"
    Just (CreateUserPayload u e p) -> do
      res <- insertUser config (UserDb 0 u e p)
      json $ case res of
        Right True -> resGood
        Left err -> resBad err
        _ -> resBad "unknown error"

authUser :: Config -> ScottyM ()
authUser config = post "/auth" $ do
  payload <- decode <$> body
  case payload of
    Nothing -> json $ resBad "bad request"
    Just (AuthPayload name pass) -> do
      res <- getUserByNameAndPass config name pass
      json res

{--                 Querying Database                    --}

getUserById :: MonadIO m => Config -> Int -> m (Maybe UserDb)
getUserById config searchId = maybeHead <$> userQuery config qry arg
  where
    qry = "Select id,username,mail,password from users where id = ?"
    arg = Only searchId

getAllUsers :: MonadIO m => Config -> m [UserDb]
getAllUsers config = userQuery config qry arg
  where
    qry = "Select id,username,mail,password from users"
    arg = ()

insertUser :: MonadIO m => Config -> UserDb -> m (Either Text Bool)
insertUser config (UserDb _ u e p) = fmap (> 0) <$> userMutation config qry arg
  where
    qry = "insert into users (username, mail, password) values (?, ?, ?)"
    arg = (u, e, p)

getUserByNameAndPass :: MonadIO m => Config -> Text -> Text -> m (Maybe UserDb)
getUserByNameAndPass config name pass = maybeHead <$> userQuery config qry arg
  where
    qry = "Select id,username,mail,password from users where username = ? and password = ?"
    arg = (name, pass)

userQuery :: (MonadIO m, ToRow a) => Config -> Query -> a -> m [UserDb]
userQuery config qry arg = withConnUnchecked (db config) $ \con -> query con qry arg

userMutation :: (MonadIO m, ToRow a) => Config -> Query -> a -> m (Either Text Int64)
userMutation config qry arg = withConn (db config) $ \con -> execute con qry arg
