{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import BasicPrelude
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.DiffMatchPatch (DiffChange(..), calculateDiff)


-- TODO: Move instances to separate packages

-- TODO: Rename to 'Test'

-- TODO: Add a Travis badge to the README.rst

-- TODO: Fix hyperlink in README.rst


instance Arbitrary Text where
  arbitrary = fromString <$> arbitrary


newtype NonEmptyText = NonEmptyText Text deriving (Eq, Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText <$> arbitrary `suchThat` (not . Text.null)
  shrink (NonEmptyText t) = [NonEmptyText t | t' <- shrink t, not (Text.null t')]


tests :: TestTree
tests =
  -- Tests for calcluteDiff function, the main entry point of the code.
  testGroup "calculateDiff"
  [ testGroup "unit tests"
    [ testCase "empty text produces empty diff" $
        calculateDiff "" "" @?= []
    ]
  , testGroup "properties"
    [ testProperty "equal text produces equal diff" $
        \(NonEmptyText x) -> calculateDiff x x === [Equal x]
    , testProperty "non-equal text produces non-equal diff" $
      \(NonEmptyText x) (NonEmptyText y) -> stripEquals (calculateDiff x y) /= []
    ]
  ]


-- | Return a list of DiffChanges with the equality objects split out.
--
-- XXX: Really should be internal to the property where it's used, but I can't
-- figure that out.
stripEquals :: [DiffChange a] -> [DiffChange a]
stripEquals = mapMaybe notEquals
  where
    notEquals (Equal _) = Nothing
    notEquals x         = Just x


main :: IO ()
main = defaultMain tests
