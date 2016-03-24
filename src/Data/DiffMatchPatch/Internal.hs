-- | Implementation of diff-match-patch

module Data.DiffMatchPatch.Internal
    ( TextPair
    , makeTextPair
    , getTexts
    , firstText
    , secondText
    , getTextCores
    , commonPrefix
    , commonSuffix
    , commonSuffixes
    ) where


import BasicPrelude
import qualified Data.Text as Text


-- | A pair of 'Text' elements.
--
-- The common prefix and the common suffix of the elements are stored
-- separately.
data TextPair = TextPair
                { _first :: Text
                , _second :: Text
                , _prefix :: Text
                , _suffix :: Text
                } deriving (Eq, Show)


-- | Construct a 'TextPair' from two 'Text' values.
makeTextPair :: Text -> Text -> TextPair
makeTextPair x y =
  let (prefix, x', y') = fromMaybe ("", x, y) (Text.commonPrefixes x y)
      (suffix, x'', y'') = fromMaybe ("", x', y') (commonSuffixes x' y')
  in TextPair x'' y'' prefix suffix


-- | /O(n_1 + n_2)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- > commonSuffixes "barfoo" "quuxfoo" == Just ("foo","bar","quux")
-- > commonSuffixes "veeble" "fetzer"  == Nothing
-- > commonSuffixes "" "baz"           == Nothing
--
-- TODO: This is a very naive implementation and is probably pretty slow. Make
-- a faster one.
commonSuffixes :: Text -> Text -> Maybe (Text, Text, Text)
commonSuffixes x y =
  case Text.commonPrefixes (Text.reverse x) (Text.reverse y) of
    Nothing -> Nothing
    Just (p, x', y') -> Just (Text.reverse p, Text.reverse x', Text.reverse y')


-- | Get the original 'Text' values from a 'TextPair'.
getTexts :: TextPair -> (Text, Text)
getTexts TextPair{..} =
  (_prefix <> _first <> _suffix, _prefix <> _second <> _suffix)


-- | Get the first 'Text' value from a 'TextPair'.
firstText :: TextPair -> Text
firstText = fst . getTexts


-- | Get the second 'Text' value from a 'TextPair'.
secondText :: TextPair -> Text
secondText = snd . getTexts


-- | Get the "cores" of the 'Text' values that make up the pair.
--
-- Guaranteed to have an empty common prefix and an empty common suffix.
getTextCores :: TextPair -> (Text, Text)
getTextCores TextPair{..} = (_first, _second)


-- | Get the common prefix of the text pair. An empty text object is returned
-- if there is no common prefix.
commonPrefix :: TextPair -> Text
commonPrefix = _prefix


-- | Get the common suffix of the text pair. An empty text object is returned
-- if there is no common suffix.
commonSuffix :: TextPair -> Text
commonSuffix = _suffix
