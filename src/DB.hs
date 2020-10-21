{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs, PolyKinds #-}
{-# LANGUAGE DataKinds, KindSignatures, FunctionalDependencies #-}
module DB(DBType(..), SomeDBType(..), SomeDBException(..),
  SourceID(..), SubjectID(..), FileID(..), User(..), Password(..), Host(..), Port(..), Database(..), ConnInfo,
  DBConn(..), getFileID, insertEAV, insertError, clearErrors,mkConnInfo) where

import           Data.Kind                     (Type)
import           Database.HDBC.PostgreSQL.Pure (Connection, Config(..))
import           Database.PostgreSQL.Pure      (ErrorResponse(..))
import           Database.MySQL.Base           (MySQLConn, ConnectInfo(..))
import qualified Database.HDBC.PostgreSQL.Pure as Postgres
import qualified Database.MySQL.Base           as MySQL
import           Database.HDBC.Types           (IConnection(commit, disconnect))
import           Data.Default.Class            (def)

import           Data.Text                     (Text)
import           Data.UUID                     (UUID)

import           DB.Types
import           DB.Utils
import qualified DB.Postgres                   as Postgres
import qualified DB.MySQL                      as MySQL
import Data.Maybe (fromMaybe)
import Data.String.Conv (toS)
import Control.Exception (catch, IOException, Exception)
import Control.Monad.Catch (MonadThrow(throwM))


data DBType :: Type -> Type where
    Postgres :: DBType Connection
    MySQL :: DBType MySQLConn


class DBConn con_ty where
  connect :: User -> Password -> Host -> Port -> Database -> IO con_ty
  disconnect :: con_ty -> IO ()
  dbType :: DBType con_ty


data SomeDBType = forall con_ty. DBConn con_ty => SomeDBType (DBType con_ty)

data SomeDBException = forall e . Show e => SomeDBException {
    simple :: String
  , original :: e
  } 

deriving instance Show SomeDBException

instance Exception SomeDBException

instance DBConn Connection where
  connect (User user) (Password password) (Host host) (Port port) (Database db) = let address = Postgres.AddressNotResolved host (fromMaybe "5432" port)
    in Postgres.connect def{address = address , database = db , user = user , password = password} 
      `catch` (\(e :: IOException) -> throwM $ SomeDBException "Failed to connect to DB."  e)
      `catch` (\e@(ErrorResponse _ _ msg _) -> throwM $ SomeDBException (toS msg) e)
  disconnect con = commit con >> Database.HDBC.Types.disconnect con
  dbType = Postgres

instance DBConn MySQLConn where
  connect (User user) (Password password) (Host host) (Port port) (Database db) = 
    MySQL.connect MySQL.defaultConnectInfo{
      ciHost = host
    , ciPort = fromMaybe 3306 $ fmap read port
    , ciDatabase = toS db
    , ciUser = toS user
    , ciPassword = toS password
    } 
      `catch` (\(e :: IOException) -> throwM $ SomeDBException "Failed to connect to DB." e)
      `catch` (\(MySQL.ERRException e@(MySQL.ERR _ _ msg)) -> throwM $ SomeDBException (toS msg) e)
  disconnect = MySQL.close
  dbType = MySQL


getFileID :: forall con. DBConn con => SourceID -> String -> con -> IO (Maybe FileID)
getFileID sID nm con = case dbType @con of
  Postgres -> Postgres.getFileID sID nm con
  MySQL -> MySQL.getFileID sID nm con

insertEAV :: forall con. DBConn con => UUID -> SourceID -> FileID -> SubjectID -> Text -> Text -> con -> IO ()
insertEAV uuID srcID fID subID a v con = case dbType @con of
  Postgres -> Postgres.insertEAV uuID srcID fID subID a v con
  MySQL -> MySQL.insertEAV uuID srcID fID subID a v con

insertError :: forall con. DBConn con => SourceID -> FileID -> String -> con -> IO ()
insertError sID fID err con = case dbType @con of
  Postgres -> Postgres.insertError sID fID err con
  MySQL -> MySQL.insertError sID fID err con

clearErrors :: forall con. DBConn con => SourceID -> FileID -> con -> IO ()
clearErrors sID fID con = case dbType @con of
  Postgres -> Postgres.clearErrors sID fID con
  MySQL -> MySQL.clearErrors sID fID con


type ConnInfo = (SomeDBType, User, Password, Host, Port, Database)

mkConnInfo :: String -> Maybe ConnInfo
mkConnInfo str = do
  (db_ty, user, pass, host, port, db) <- parseConnString str
  case db_ty of
    "mysql" -> Just (SomeDBType MySQL, user, pass, host, port, db)
    "postgres" -> Just (SomeDBType Postgres, user, pass, host, port, db)
    "postgresql" -> Just (SomeDBType Postgres, user, pass, host, port, db)
    _ -> Nothing