module Utils
  ( maybeHead,
  )
where

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a : _) = Just a
