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

import           DB                           (DBConfig)

-- adapted from the https://hackage.haskell.org/package/postgresql-simple-url library

uriToConfig :: URI -> Maybe DBConfig
uriToConfig uri
  | uriScheme uri == "postgres:" || uriScheme uri == "postgresql:" = (Left . ($ def)) <$> mkConfig uri
  | uriScheme uri == "mysql:" = (postgresToMySQLConfig . ($ def)) <$> mkConfig uri
  | otherwise = Nothing

type ConfigChange = Config -> Config

mkConfig :: URI -> Maybe ConfigChange
mkConfig uri = case uriPath uri of
  ('/' : rest) | not (null rest) -> Just $ uriParameters uri
  _                              -> Nothing

uriParameters :: URI -> ConfigChange
uriParameters uri = (\info -> info { database = tail $ uriPath uri }) . maybe id uriAuthParameters (uriAuthority uri)

dropLast :: [a] -> [a]
dropLast []     = []
dropLast [_]    = []
dropLast (x:xs) = x : dropLast xs

uriAuthParameters :: URIAuth -> ConfigChange
uriAuthParameters uriAuth = port . host . auth
  where port = case uriPort uriAuth of
                 (':' : p) -> \info -> info { address = case address info of 
                   AddressResolved _ -> AddressNotResolved "" p 
                   AddressNotResolved host _ -> AddressNotResolved host p }
                 _         -> id
        host = case uriRegName uriAuth of
                 h  -> \info -> info { address = case address info of 
                   AddressResolved _ -> AddressNotResolved h "" 
                   AddressNotResolved _ port -> AddressNotResolved h port }
        auth = case splitOn ":" (uriUserInfo uriAuth) of
                 [""]   -> id
                 [u]    -> \info -> info { user = dropLast u }
                 [u, p] -> \info -> info { user = u, password = dropLast p }
                 _      -> id


postgresToMySQLConfig :: Config -> DBConfig
postgresToMySQLConfig (Config (AddressNotResolved host port) user pass db _ _ _ _) = Right $ 
  defaultConnectInfo {
    ciHost = host
  , ciPort = read port
  , ciDatabase = toS db
  , ciUser = toS user 
  , ciPassword = toS pass
  }

newtype DBConfigArg = DBConfigArg [DBConfig]
  deriving Typeable

instance IsOption DBConfigArg where
  defaultValue = DBConfigArg []
  parseValue v = DBConfigArg <$> (forM (splitOn ";" v) $ \s -> parseURI s >>= uriToConfig)
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