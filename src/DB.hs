module DB(DBType(..), SourceID(..), SubjectID(..), FileID(..), DBConfig, DBConn, getFileID, insertEAV, insertError, clearErrors) where

import qualified Database.HDBC.PostgreSQL.Pure as Postgres
import qualified Database.MySQL.Base           as MySQL
import           Data.Text                     (Text)
import           Data.UUID                     (UUID)

import           DB.Types
import qualified DB.Postgres                   as Postgres
import qualified DB.MySQL                      as MySQL


data DBType = Postgres | MySQL


type DBConfig = Either Postgres.Config MySQL.ConnectInfo

type DBConn = Either Postgres.Connection MySQL.MySQLConn

getFileID :: SourceID -> String -> DBConn -> IO (Maybe FileID)
getFileID sID nm (Left con) = Postgres.getFileID sID nm con
getFileID sID nm (Right con) = MySQL.getFileID sID nm con

insertEAV :: UUID -> SourceID -> FileID -> SubjectID -> Text -> Text -> DBConn -> IO ()
insertEAV uuID srcID fID subID a v (Left con) = Postgres.insertEAV uuID srcID fID subID a v con
insertEAV uuID srcID fID subID a v (Right con) = MySQL.insertEAV uuID srcID fID subID a v con

insertError :: SourceID -> FileID -> String -> DBConn -> IO ()
insertError sID fID err (Left con) = Postgres.insertError sID fID err con
insertError sID fID err (Right con) = MySQL.insertError sID fID err con

clearErrors :: SourceID -> FileID -> DBConn -> IO ()
clearErrors sID fID (Left con) = Postgres.clearErrors sID fID con
clearErrors sID fID (Right con) = MySQL.clearErrors sID fID con
