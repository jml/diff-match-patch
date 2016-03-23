-- | Command line debugging tool for development of diff-match-patch.
--
-- Not actually expected to ship any useful functionality. Just present to
-- make debugging easier. Might be deleted at any time.

module Main (main) where

import BasicPrelude
import Data.DiffMatchPatch (calculateDiff)

main :: IO ()
main = print $ calculateDiff "a" "b"
