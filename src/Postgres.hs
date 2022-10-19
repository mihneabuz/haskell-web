module Postgres
  ( getPostgres,
    withConn
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

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

withConn :: MonadIO m => Pool Connection -> (Connection -> IO a) -> m a
withConn pool action = do
  liftIO $ withResource pool action
