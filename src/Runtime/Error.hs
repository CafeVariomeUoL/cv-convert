{-# LANGUAGE ExistentialQuantification, UndecidableInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Runtime.Error where

import           Data.Aeson                    (Value(..), ToJSON(..), GToJSON, encode, toJSON, genericToJSON, defaultOptions, tagSingleConstructors, sumEncoding, defaultTaggedObject, tagFieldName, Zero)
import           Data.Typeable                 (Typeable, cast)
import           GHC.Generics
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Catch           (MonadThrow(..), Exception, SomeException, toException, fromException)
import           System.IO                     (hPutStrLn)
import           System.FilePath.Posix         (takeFileName)
-- import           Database.HDBC.Types(SqlError)
import qualified Data.Text                     as T
import qualified Data.HashMap.Strict           as HM
import           Data.String.Conv              (toS)

import           Runtime.Types
import           DB




handleError :: (MonadThrow m, MonadIO m) =>
  ErrorHandling -> Int -> SomeRuntimeException -> Value -> Value -> m ()
handleError (ErrorHandling (Terminate   , _                               )) lineNo err inp out = 
  throwM $ RowError lineNo err inp out
handleError (ErrorHandling (LogToConsole, _                               )) lineNo err inp out = 
  liftIO $ putStrLn $ show $ RowError lineNo err inp out
handleError (ErrorHandling (_         , Just (JSONFileOutput logFile)   )) lineNo err inp out = 
  liftIO $ hPutStrLn logFile $ show $ RowError lineNo err inp out
handleError (ErrorHandling (_         , Just (DBOutput con srcID fileID))) lineNo err inp out = liftIO $ do
  putStrLn $ show $ RowError lineNo err inp out
  insertError srcID fileID (show $ RowError lineNo err inp out) con 
handleError (ErrorHandling (_         , _                               )) lineNo err inp out = 
  liftIO $ putStrLn $ show $ RowError lineNo err inp out

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


data RuntimeError = RuntimeError String 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException RuntimeError)
  deriving Exception via (RuntimeException RuntimeError)

instance Show RuntimeError where
  show (RuntimeError e) = e
