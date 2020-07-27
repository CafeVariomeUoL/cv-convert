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
import           Runtime
import           Quickjs



lib_js = [i|let Utils = {
  helloWorld: function() {return 1;}
}
export default Utils;|]


load_lib :: Assertion
load_lib = do
    writeFile "./test-lib.js" lib_js
    quickjs $ do
      loadLibrary "./test-lib.js"
      v <- eval "Utils.helloWorld();"
      liftIO $ v @?= Number 1



tests :: TestTree
tests = 
  testGroup "Runtime"
    [ testCase "call loadLibrary and evaluate 'Utils.helloWorld();'" load_lib
    ]