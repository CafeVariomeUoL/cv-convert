{-# LANGUAGE ExistentialQuantification, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Runtime.Error where

import           Data.Aeson                   (Value(..), ToJSON(..), FromJSON(..), GToJSON, toJSON, genericParseJSON, genericToJSON,  defaultOptions, constructorTagModifier, camelTo2, tagSingleConstructors, sumEncoding, defaultTaggedObject, tagFieldName, Zero)
import           Data.Typeable                (Typeable, cast)
import           GHC.Generics
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Catch          (MonadThrow(..), Exception, SomeException, toException, fromException)
import           System.IO                    (Handle, hPutStrLn)
import           System.FilePath.Posix        (takeFileName)
import           Database.HDBC.PostgreSQL.Pure(Connection)
-- import           Database.HDBC.Types(SqlError)
import qualified Data.Text                    as T
import qualified Data.HashMap.Strict          as HM
import           Data.String.Conv             (toS)

import           DB



{-|
Data type used to describe the error reporting/handling behaviour when processing a row. 
Other than 'Terminate, all other options result in the error from a row being recorded/printed,
with the computation continuing onto the next row.
-}
data ErrorOpts = Terminate | LogToConsole | LogToFile | LogToDb deriving (Show, Eq, Generic)

instance FromJSON ErrorOpts where
  parseJSON = genericParseJSON defaultOptions {
                constructorTagModifier = camelTo2 '_' }



data SomeRuntimeException = forall e . (Exception e, ToJSON e) => SomeRuntimeException e 

instance Show SomeRuntimeException where
  show (SomeRuntimeException e) = show e
instance ToJSON SomeRuntimeException where
  toJSON (SomeRuntimeException e) = toJSON e

instance Exception SomeRuntimeException

runtimeExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
runtimeExceptionToException = toException . SomeRuntimeException

runtimeExceptionFromException :: Exception e => SomeException -> Maybe e
runtimeExceptionFromException x = do
  SomeRuntimeException a <- fromException x
  cast a


newtype RuntimeException e = RuntimeException e 
  deriving Generic
  deriving newtype Show

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (RuntimeException a) where
  toJSON (RuntimeException a) = genericToJSON opts a
    where
      opts = defaultOptions { tagSingleConstructors = True
                            , sumEncoding = defaultTaggedObject{ tagFieldName = "error_type"}
                            }

instance (Show e, Typeable e, Generic e, GToJSON Zero (Rep e)) => Exception (RuntimeException e)
  where
    toException = runtimeExceptionToException
    fromException = runtimeExceptionFromException


data SubjectIDNotFound = SubjectIDNotFound 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException SubjectIDNotFound)
  deriving Exception via (RuntimeException SubjectIDNotFound)

instance Show SubjectIDNotFound where
  show _ = "subject_id is required"


data SheetNotFound = SheetNotFound {name :: T.Text} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException SheetNotFound)
  deriving Exception via (RuntimeException SheetNotFound)

instance Show SheetNotFound where
  show SheetNotFound{..} = "Sheet '" ++ toS name ++ "' does not exist in the current file."


data CSVParseError = CSVParseError {message :: String} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException CSVParseError)
  deriving Exception via (RuntimeException CSVParseError)

instance Show CSVParseError where
  show CSVParseError{..} = message


data FileIDNotFound = FileIDNotFound {file :: String} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException FileIDNotFound)
  deriving Exception via (RuntimeException FileIDNotFound)

instance Show FileIDNotFound where
  show FileIDNotFound {..} = "File ID for '" ++ (takeFileName file) ++ "' could not be found."


data RowError = forall e . (Exception e, ToJSON e) => RowError {row :: Int, _error :: e} 

instance Show RowError where
  show RowError{..} = "Error on line " ++ show row ++ ": " ++ show _error

instance ToJSON RowError where
  toJSON RowError{..} = Object $ HM.fromList [("error_type", String "RowError"), ("row", toJSON row), ("error", toJSON _error)]

instance Exception RowError where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException



handleError :: (MonadThrow m, MonadIO m) =>
  ErrorOpts -> Maybe Handle -> Maybe (Connection, SourceID, FileID) -> Int -> SomeRuntimeException -> m ()
handleError Terminate _ _ lineNo err = 
  throwM $ RowError lineNo err
handleError LogToConsole _ _ lineNo err = 
  liftIO $ putStrLn $ "Error on line " ++ show lineNo ++ ": " ++ show err
handleError LogToFile (Just logFile) _ lineNo err = 
  liftIO $ hPutStrLn logFile $ "Error in row " ++ show lineNo ++ ": " ++ show err
handleError LogToDb _ (Just (con, srcID, fileID)) lineNo err = liftIO $ do
  putStrLn $ "Error in row " ++ show lineNo ++ ": " ++ show err
  insertError srcID fileID ("Error in row " ++ show lineNo ++ ": " ++ show err) con 
handleError LogToDb _ Nothing lineNo err = 
  throwM $ RowError lineNo err