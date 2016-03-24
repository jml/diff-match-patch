-- | Implementation of diff, match, and patch.
--
-- Based on Neil Fraser's work, as found at
-- https://code.google.com/archive/p/google-diff-match-patch/

{-# LANGUAGE ViewPatterns #-}

module Data.DiffMatchPatch
    ( DiffChange(..)
    , calculateDiff
    ) where

import BasicPrelude hiding (insert, delete)
import qualified Data.Text as Text

import Data.DiffMatchPatch.Internal ( TextPair
                                    , makeTextPair
                                    , commonPrefix
                                    , commonSuffix
                                    , getTextCores
                                    )


-- | A single change in a diff. A full diff is a sequence of these.
data DiffChange a = Delete a | Insert a | Equal a deriving (Eq, Show)

change :: (Text -> DiffChange Text) -> Text -> Maybe (DiffChange Text)
change f x
  | Text.null x = Nothing
  | otherwise   = Just (f x)


insert' :: Text -> Maybe (DiffChange Text)
insert' = change Insert

insert :: Text -> [DiffChange Text]
insert  = maybeToList . insert'

delete' :: Text -> Maybe (DiffChange Text)
delete' = change Delete

delete :: Text -> [DiffChange Text]
delete  = maybeToList . delete'

equal' :: Text -> Maybe (DiffChange Text)
equal'  = change Equal

equal :: Text -> [DiffChange Text]
equal   = maybeToList . equal'


-- | Calculate the difference between two texts.
--
-- XXX: The only reason this is 'Text' is because we need a null check. Could
-- maybe use AsEmpty prism from Control.Lens.
calculateDiff :: Text -> Text -> [DiffChange Text]
calculateDiff x y
  | x == y    = equal x
  | otherwise =
      let pair = makeTextPair x y
      in equal (commonPrefix pair) <> computeDiff pair <> equal (commonSuffix pair)


computeDiff :: TextPair -> [DiffChange Text]
computeDiff (getTextCores -> (x, y))
  | Text.null x = insert y
  | Text.null y = delete x
  | otherwise   =
    let (swapped, (shorter, longer)) = ordered Text.length (x, y)
    in
      case extractInfix shorter longer of
        -- 'shorter' is inside 'longer'
        Just (prefix, suffix) ->
          case swapped of
            -- x is shorter, and thus inside y
            NotSwapped -> [Insert prefix, Equal shorter, Insert suffix]
            -- y is shorter, and thus inside x
            Swapped    -> [Delete prefix, Equal shorter, Delete suffix]
        Nothing ->
          if Text.length shorter == 1
          then
            -- TextPair guarantees that x and y are either both empty or never
            -- equal
            [Delete x, Insert y]
          else
            error "TODO: half-match; line mode; bisect"


-- | If `needle` is inside `haystack`, return the bit before `needle` and the
-- bit after it. Otherwise, return 'Nothing'.
--
-- If `needle` occurs multiple times inside `haystack`, just break on the
-- first occurrence.
--
-- Laws:
--
--   If `extractInfix x y` returns `Just (a, b)`, then a <> x <> b == y
--   If `extractInfix x y` returns `Nothing`, then there is no `a` and `b`
--   such that a <> x <> b == y
--
-- Examples:
--
-- > extractInfix "foo" "bananafooapple" == Just ("banana", "apple")
-- > extractInfix "foo" "bananaapple" == Nothing
extractInfix :: Text -> Text -> Maybe (Text, Text)
extractInfix needle haystack =
  case Text.breakOn needle haystack of
    (_, "") -> Nothing
    (prefix, suffix) -> Just (prefix, Text.drop (Text.length needle) suffix)


data Swap = NotSwapped | Swapped

ordered :: Ord b => (a -> b) -> (a, a) -> (Swap, (a, a))
ordered f (x, y) = if f x < f y then (NotSwapped, (x, y)) else (Swapped, (y, x))
