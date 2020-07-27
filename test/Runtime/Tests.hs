module Runtime.Tests(tests) where
import           Test.Tasty              (TestTree, testGroup)
-- import           Test.Tasty.HUnit        (testCase)
-- import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
-- import           Test.HUnit              (Assertion, (@?=))
-- import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
-- import           Data.Aeson              (Value(..))
-- import           Control.Monad.IO.Class  (liftIO)
-- import           Control.Monad.Catch     (SomeException, MonadCatch(..))
-- import           Data.Text               (pack)
-- import qualified Data.HashMap.Strict            as HS
-- import qualified Data.Vector             as V
-- import           Quickjs


-- eval_1_plus_2 :: Assertion
-- eval_1_plus_2 = quickjs $ do
--   v <- eval "1+2"
--   liftIO $ v @?= Number 3


tests :: TestTree
tests = 
  testGroup "Runtime"
    [ 
    ]