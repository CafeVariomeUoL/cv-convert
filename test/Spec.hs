module Main where

import           Test.Tasty                   (defaultMainWithIngredients, defaultIngredients, includingOptions, askOption, testGroup)
import           Test.Tasty.Options
import           Test.Tasty.Golden            (DeleteOutputFile)
import qualified Runtime.Tests
import qualified JSON.Utils.Tests
import           Database.HDBC.PostgreSQL.Pure(Config(..), Address(..))
import           Database.MySQL.Base          (defaultConnectInfo, ConnectInfo(..))
import           Network.URI                  (URI, URIAuth, parseURI, uriPath, uriAuthority, uriScheme, uriPort, uriRegName, uriUserInfo)
import           Data.Default.Class           (def)
import           Data.Proxy
import           Data.Typeable
import           Data.List.Split              (splitOn)
import           Data.String.Conv             (toS)
import           Control.Monad                (forM)
import           Data.List.Split              (splitOn)

import           DB                           (mkConnInfo, ConnInfo)


newtype DBConfigArg = DBConfigArg [ConnInfo]
  deriving Typeable

instance IsOption DBConfigArg where
  defaultValue = DBConfigArg []
  parseValue v = DBConfigArg <$> forM (splitOn ";" v) mkConnInfo
  optionName = return "db-config"
  optionHelp = return "DB config passed in as a postgres:// or mysql:// url"


main :: IO ()
main =  do
  runtimeTests <- Runtime.Tests.tests
  defaultMainWithIngredients (optsIng : defaultIngredients) $
    askOption $ \(DBConfigArg config) -> testGroup "Tests"
      [ runtimeTests config
      , JSON.Utils.Tests.tests
      ]
  where
    optsIng = includingOptions [
        Option (Proxy :: Proxy DBConfigArg)
      , Option (Proxy :: Proxy DeleteOutputFile)
      ]