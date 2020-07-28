{-# LANGUAGE QuasiQuotes #-}

module Runtime.Tests(tests) where
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (testCase)
-- import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=))
-- import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..))
import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.Catch     (SomeException, MonadCatch(..))
-- import           Data.Text               (pack)
-- import qualified Data.HashMap.Strict            as HS
-- import qualified Data.Vector             as V
import           Data.String.Interpolate      (i)
import           Main.Utf8                    (withUtf8)

import           Runtime
import           Quickjs



lib_js = [i|let Utils = {
  helloWorld: function() {return 2;}
}
export default Utils;|]


load_lib :: Assertion
load_lib =do
    withUtf8 $ writeFile "./test-lib.js" lib_js
    quickjs $ do
      loadLibrary "./test-lib.js"
      v <- eval "Utils.helloWorld();"
      liftIO $ v @?= Number 2



tests :: TestTree
tests = 
  testGroup "Runtime"
    [ testCase "call loadLibrary and evaluate 'Utils.helloWorld();'" load_lib
    ]