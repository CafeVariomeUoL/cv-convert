module Main where

import           Test.Tasty           (defaultMain, testGroup)
import qualified Quickjs.Tests
import qualified Runtime.Tests
import qualified JSON.Utils.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Quickjs.Tests.tests
    , Runtime.Tests.tests
    , JSON.Utils.Tests.tests
    ]