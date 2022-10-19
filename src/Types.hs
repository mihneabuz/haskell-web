module Types
  ( Config (..),
  )
where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

newtype Config = Config {db :: Pool Connection}
