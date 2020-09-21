module DB.Utils(parseConnString) where

import Data.List (elemIndex)
import Network.URI (uriPort, URIAuth, parseURI, uriScheme, uriPath, uriAuthority, uriUserInfo, uriRegName)
import DB.Types


parseAuthString :: Maybe URIAuth -> Maybe (User, Password, Host, Port)
parseAuthString mas =  do 
  as <- mas
  i <- elemIndex ':' $ uriUserInfo as
  let (user, pass) = splitAt i $ uriUserInfo as
  let host = uriRegName as
  let port = drop 1 $ uriPort as
  return (User user, Password $ init $ tail pass, Host host, Port $ if length port > 0 then Just port else Nothing)


parseConnString :: String -> Maybe (String, User, Password, Host, Port, Database)
parseConnString cs = do
  uri <- parseURI cs
  db <- return $ init $ uriScheme uri
  path <- return $ Database $ drop 1 $ uriPath uri
  (user, pass, host, port) <- parseAuthString $ uriAuthority uri
  return (db, user, pass, host, port, path)