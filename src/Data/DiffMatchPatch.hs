module Data.DiffMatchPatch
    ( DiffChange(..)
    , calculateDiff
    ) where

import BasicPrelude
import qualified Data.Text as Text


-- | A single change in a diff. A full diff is a sequence of these.
data DiffChange a = Delete a | Insert a | Equal a deriving (Eq, Show)


-- | Calculate the difference between two texts.
--
-- XXX: The only reason this is 'Text' is because we need a null check. Could
-- maybe use AsEmpty prism from Control.Lens.
calculateDiff :: Text -> Text -> [DiffChange Text]
calculateDiff x y
  | x == y    = if Text.null x then [] else [Equal x]
  | otherwise = error "TODO: Actual diffing not implemented yet"



data TwoTexts = TwoTexts
                { first :: Text
                , second :: Text
                , prefix :: Text
                , suffix :: Text
                }


optimizeEnds :: Text -> Text -> TwoTexts
optimizeEnds first second =
  case Text.commonPrefixes first second of
    Nothing -> TwoTexts first second "" ""
    Just (p, f, s) -> TwoTexts first second p ""


getTexts :: TwoTexts -> (Text, Text)
getTexts (TwoTexts first second prefix suffix) =
  (prefix <> first <> suffix, prefix <> second <> suffix)
