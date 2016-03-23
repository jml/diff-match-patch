{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import BasicPrelude
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.DiffMatchPatch (DiffChange(..), calculateDiff)


instance Arbitrary Text where
  arbitrary = fromString <$> arbitrary


newtype NonEmptyText = NonEmptyText { getNonEmptyText :: Text } deriving (Eq, Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText <$> arbitrary `suchThat` (not . Text.null)
  shrink (NonEmptyText t) = [NonEmptyText t | t' <- shrink t, not (Text.null t')]


tests :: TestTree
tests =
  testGroup "calculateDiff"
  [ testGroup "unit tests"
    [ testCase "empty text produces empty diff" $
        calculateDiff "" "" @?= []
    ]
  , testGroup "properties"
    [ testProperty "equal text produces equal diff" $
        \x -> not (Text.null x) ==> (calculateDiff x x === [Equal x])
    , testProperty "non-equal text produces non-equal diff" $
      \x y -> stripEquals (calculateDiff (getNonEmptyText x) (getNonEmptyText y)) /= []
    ]
  ]


stripEquals :: [DiffChange a] -> [DiffChange a]
stripEquals = mapMaybe notEquals
  where
    notEquals (Equal _) = Nothing
    notEquals x         = Just x


main :: IO ()
main = defaultMain tests
