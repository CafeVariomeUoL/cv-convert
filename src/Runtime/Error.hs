{-# LANGUAGE ExistentialQuantification, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Runtime.Error where

import           Data.Aeson                   (Value(..), ToJSON(..), FromJSON(..), GToJSON, encode, toJSON, genericToJSON, defaultOptions, tagSingleConstructors, sumEncoding, defaultTaggedObject, tagFieldName, Zero, withText)
import           Data.Aeson.Types             (unexpected)
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
data ErrorOpts fileOpts dbOpts = Terminate | LogToConsole | LogToFile fileOpts | LogToDb dbOpts deriving (Show, Eq, Generic)


instance FromJSON (ErrorOpts () ()) where 
  parseJSON = withText "ErrorOpts" $ \case 
    s | s == "terminate"  -> return Terminate
    s | s == "log_to_console" -> return LogToConsole
    s | s == "tog_to_file"  -> return $ LogToFile ()
    s | s == "log_to_db" -> return $ LogToDb ()
    s | otherwise   -> unexpected $ String s


withLogToFileErrorOpt :: MonadIO m => ErrorOpts a b -> (a -> m fileOpts) -> m (ErrorOpts fileOpts b)
withLogToFileErrorOpt (LogToFile a) f = f a >>= return . LogToFile
withLogToFileErrorOpt (LogToDb o)   _ = pure $ LogToDb o
withLogToFileErrorOpt Terminate     _ = pure Terminate
withLogToFileErrorOpt LogToConsole  _ = pure LogToConsole

withLogToDBErrorOpt :: MonadIO m => ErrorOpts a b -> (b -> m dbOpts) -> m (ErrorOpts a dbOpts)
withLogToDBErrorOpt (LogToFile o) _ = pure $ LogToFile o
withLogToDBErrorOpt (LogToDb b)   f = f b >>= return . LogToDb
withLogToDBErrorOpt Terminate     _ = pure Terminate
withLogToDBErrorOpt LogToConsole  _ = pure LogToConsole

withLogToDBErrorOpt_ :: MonadIO m => ErrorOpts a b -> m () -> m ()
withLogToDBErrorOpt_ e m = withLogToDBErrorOpt e (const m) >> return ()

handleError :: (MonadThrow m, MonadIO m) =>
  ErrorOpts Handle (Connection, SourceID, FileID) -> Int -> SomeRuntimeException -> Value -> Value -> m ()
handleError Terminate                      lineNo err inp out = 
  throwM $ RowError lineNo err inp out
handleError LogToConsole                   lineNo err inp out = 
  liftIO $ putStrLn $ show $ RowError lineNo err inp out
handleError (LogToFile logFile)            lineNo err inp out = 
  liftIO $ hPutStrLn logFile $ show $ RowError lineNo err inp out
handleError (LogToDb (con, srcID, fileID)) lineNo err inp out = liftIO $ do
  putStrLn $ show $ RowError lineNo err inp out
  insertError srcID fileID (show $ RowError lineNo err inp out) con 


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
                            , sumEncoding = defaultTaggedObject{ tagFieldName = "error_type" }
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


data RowError = forall e . (Exception e, ToJSON e) => RowError {row :: Int, _error :: e, input :: Value, output :: Value} 

instance Show RowError where
  show RowError{..} = "Error on line " ++ show row ++ ": " ++ show _error ++ "\nInput: " ++ toS (encode input) ++ "\nOutput: " ++ toS (encode output)

instance ToJSON RowError where
  toJSON RowError{..} = Object $ HM.fromList [("error_type", String "RowError"), ("row", toJSON row), ("error", toJSON _error), ("input", toJSON output), ("output", toJSON output)]

instance Exception RowError where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException

data HashMismatch = HashMismatch {url :: T.Text, expected_hash :: T.Text, found_hash :: T.Text} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException HashMismatch)
  deriving Exception via (RuntimeException HashMismatch)

instance Show HashMismatch where
  show HashMismatch{..} = toS $ "The hash for '" <> url <> "' is incorrect.\nExpected: " <> expected_hash <> "\nFound:    " <> found_hash