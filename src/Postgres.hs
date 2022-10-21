module Postgres
  ( getPostgres,
    withConn,
    withConnUnchecked
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.Errors

pg :: ConnectInfo
pg =
  defaultConnectInfo
    { connectHost = "localhost",
      connectUser = "postgres",
      connectPassword = "password"
    }

getPostgres :: IO (Pool Connection)
getPostgres = do
  pool <- createPool (connect pg) close 1 10 10
  migrate pool
  return pool

migrate :: Pool Connection -> IO ()
migrate pool = withResource pool $ \conn -> void $ runMigrations False conn cmd
  where
    cmd = [MigrationInitialization, MigrationDirectory "postgresql"]

withConnUnchecked :: MonadIO m => Pool Connection -> (Connection -> IO a) -> m a
withConnUnchecked pool = liftIO . withResource pool

withConn :: MonadIO m => Pool Connection -> (Connection -> IO a) -> m (Either Text a)
withConn pool action = liftIO . catchViolation catcher $ Right <$> withResource pool action

catcher :: SqlError -> ConstraintViolation -> IO (Either Text a)
catcher _ violation = return . Left $ case violation of
  (UniqueViolation field) -> "field '" <> decodeUtf8 field <> "' not unique"
  (NotNullViolation field) -> "field '" <> decodeUtf8 field <> "' is null"
  _ -> "unknown error"
