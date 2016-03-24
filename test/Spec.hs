{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import BasicPrelude
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.DiffMatchPatch (DiffChange(..), calculateDiff)
import Data.DiffMatchPatch.Internal


-- TODO: Move instances to separate packages.

-- TODO: Rename to 'Test'

-- TODO: Add a Travis badge to the README.rst

-- TODO: Fix hyperlink in README.rst


instance Arbitrary Text where
  arbitrary = fromString <$> arbitrary


newtype NonEmptyText = NonEmptyText Text deriving (Eq, Show)

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText <$> arbitrary `suchThat` (not . Text.null)
  shrink (NonEmptyText t) = [NonEmptyText t | t' <- shrink t, not (Text.null t')]


instance Arbitrary TextPair where

  -- | Construct an arbitrary 'TextPair'. Pair is quite likely to have common
  -- prefix & suffix.
  arbitrary = do
    prefix <- arbitrary
    suffix <- arbitrary
    let surround x = prefix <> x <> suffix
    makeTextPair <$> (surround <$> arbitrary)
                 <*> (surround <$> arbitrary)


-- | Tests for TextPair logic.
textPairTests :: TestTree
textPairTests =
  testGroup "TextPair"
  [ testGroup "properties"
    [ testProperty "getTexts extracts original pair" $
      \x y -> getTexts (makeTextPair x y) === (x, y)

    , testProperty "cores start differently" $
      \pair -> case getTextCores pair of
        ("", "") -> True
        (_,  "") -> True
        ("", _)  -> True
        (x,  y)  -> Text.head x /= Text.head y

    , testProperty "cores end differently" $
      \pair -> case getTextCores pair of
        ("", "") -> True
        (_,  "") -> True
        ("", _)  -> True
        (x,  y)  -> Text.last x /= Text.last y

    , testProperty "prefix <> core <> suffix === original" $
      \x y -> let pair     = makeTextPair x y
                  (x', y') = getTextCores pair
                  prefix   = commonPrefix pair
                  suffix   = commonSuffix pair
              in prefix <> x' <> suffix === x .&&. prefix <> y' <> suffix === y

    , testProperty "cores only equal iff empty" $
      \(getTextCores -> (x, y)) -> x /= y .||. (Text.null x .&&. Text.null y)
    ]
    , testGroup "unit tests"
      [ testCase "commonSuffixes happy example" $
        commonSuffixes "barfoo" "quuxfoo" @?= Just ("foo", "bar", "quux")
      , testCase "commonSuffixes nothing in common" $
        commonSuffixes "veeble" "fetzer" @?= Nothing
      , testCase "commonSuffixes first empty" $
        commonSuffixes "" "baz" @?= Nothing
      ]
  ]


-- | Tests for calcluteDiff function, the main entry point of the code.
calculateDiffTests :: TestTree
calculateDiffTests =
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

  where
    -- | Return a list of DiffChanges with the equality objects split out.
    stripEquals :: [DiffChange a] -> [DiffChange a]
    stripEquals = mapMaybe notEquals
      where
        notEquals (Equal _) = Nothing
        notEquals x         = Just x


tests :: TestTree
tests =
  testGroup "All tests"
  [ calculateDiffTests
  , textPairTests
  ]



main :: IO ()
main = defaultMain tests
