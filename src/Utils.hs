module Utils
  ( maybeHead,
    resGood,
    resBad,
  )
where

import Types
import Data.Text (Text)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : _) = Just a

resGood :: Response
resGood = Response True "ok"

resBad :: Text -> Response
resBad = Response False
