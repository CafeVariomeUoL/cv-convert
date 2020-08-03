module Main where

import           Test.Tasty                   (defaultMainWithIngredients, defaultIngredients, includingOptions, askOption, testGroup)
import           Test.Tasty.Options
import           Test.Tasty.Golden            (DeleteOutputFile)
import qualified Quickjs.Tests
import qualified Runtime.Tests
import qualified JSON.Utils.Tests
import           Database.HDBC.PostgreSQL.Pure(Config(..), Address(..))
import           Network.URI                  (URI, URIAuth, parseURI, uriPath, uriAuthority, uriScheme, uriPort, uriRegName, uriUserInfo)
import           Data.Default.Class           (def)
import           Data.Proxy
import           Data.Typeable
import           Data.List.Split              (splitOn)

-- adapted from the https://hackage.haskell.org/package/postgresql-simple-url library

uriToConfig :: URI -> Maybe Config
uriToConfig uri
  | uriScheme uri /= "postgres:" && uriScheme uri /= "postgresql:" = Nothing
  | otherwise = ($ def) <$> mkConfig uri

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

newtype PostgresDBConfig = PostgresDBConfig (Maybe Config)
  deriving Typeable

instance IsOption PostgresDBConfig where
  defaultValue = PostgresDBConfig Nothing
  parseValue v = parseURI v >>= Just . PostgresDBConfig . uriToConfig
  optionName = return "postgres-db-config"
  optionHelp = return "Postgres DB config passed in as a postgres:// url"


main :: IO ()
main =  do
  runtimeTests <- Runtime.Tests.tests
  defaultMainWithIngredients (optsIng : defaultIngredients) $
    askOption $ \(PostgresDBConfig config) -> testGroup "Tests"
      [ Quickjs.Tests.tests
      , runtimeTests config
      , JSON.Utils.Tests.tests
      ]
  where
    optsIng = includingOptions [
        Option (Proxy :: Proxy PostgresDBConfig)
      , Option (Proxy :: Proxy DeleteOutputFile)
      ]