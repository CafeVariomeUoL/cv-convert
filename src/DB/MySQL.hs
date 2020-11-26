module DB.MySQL(
  -- * Inserting records into the database
  insertEAV,
  insertEAVPrepareQuery,
  -- * Storing error log output
  insertError, 
  clearErrors,
  -- * Misc
  getFileID
) where

import           Database.MySQL.Base
import qualified System.IO.Streams   as Streams
import           Data.Text           (Text, pack)
import           Data.UUID           (UUID, toText)

import           DB.Types


insertEAV :: UUID -> SourceID -> FileID -> SubjectID -> Text -> Text -> MySQLConn -> IO ()
insertEAV uuID srcID fID subID attr val con = execute_ con (insertEAVPrepareQuery uuID srcID fID subID attr val) >> return ()


insertEAVPrepareQuery :: UUID -> SourceID -> FileID -> SubjectID -> Text -> Text -> Query
insertEAVPrepareQuery uuID (SourceID srcID) (FileID fID) (SubjectID subID) attr val = renderParams
  "INSERT INTO eavs(uid, source_id, fileName, subject_id, type, attribute, value, elastic) VALUES (?, ?, ?, ?, 'attribute', ?, ?, 0)"
  [MySQLText $ toText uuID, MySQLInt32 $ fromInteger $ toInteger srcID, MySQLInt64 $ fromInteger $ toInteger fID, MySQLText $ pack $ subID, MySQLText attr, MySQLText val]


insertError :: SourceID -> FileID -> String -> MySQLConn -> IO ()
insertError (SourceID srcID) (FileID fID) msg con = execute con
  "INSERT INTO upload_error(source_id, error_id, message, error_code) VALUES (?, ?, ?, 0)"
  [MySQLInt64 $ fromInteger $ toInteger srcID,  MySQLInt64 $ fromInteger $ toInteger fID, MySQLText $ pack msg] >> return ()


clearErrors :: SourceID -> FileID -> MySQLConn -> IO ()
clearErrors (SourceID srcID) (FileID fID) con = execute con 
  "DELETE FROM upload_error WHERE source_id = ? AND error_id = ?"
  [MySQLInt64 $ fromInteger $ toInteger srcID, MySQLInt64 $ fromInteger $ toInteger fID] >> return ()


getFileID :: SourceID -> String -> MySQLConn -> IO (Maybe FileID)
getFileID (SourceID sID) nm con = do 
  (_, res) <- query con 
                    "SELECT ID FROM uploaddatastatus WHERE source_id = ? AND FileName = ?" 
                    [MySQLInt32 $ fromInteger $ toInteger sID, MySQLText $ pack nm]
  output <- Streams.read res >>= \case
    Just [MySQLInt8U fID]  -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt8 fID]   -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt16U fID] -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt16 fID]  -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt32U fID] -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt32 fID]  -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt64U fID] -> return $ Just $ FileID $ fromInteger $ toInteger fID
    Just [MySQLInt64 fID]  -> return $ Just $ FileID $ fromInteger $ toInteger fID
    _ -> return Nothing
  skipToEof res
  return output