module Data.DiffMatchPatch
    ( DiffChange(..)
    , calculateDiff
    ) where

import BasicPrelude
import qualified Data.Text as Text


-- | A single change in a diff. A full diff is a sequence of these.
data DiffChange a = Delete a | Insert a | Equal a deriving (Eq, Show)


-- | Calculate the difference between two texts.
calculateDiff :: Text -> Text -> [DiffChange Text]
calculateDiff x y
  | x == y    = if Text.null x then [] else [Equal x]
  | otherwise = error "TODO: Actual diffing not implemented yet"
