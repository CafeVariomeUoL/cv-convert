{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Runtime.Error where

import           Data.Aeson                   (Value(..), ToJSON(..), FromJSON(..), toJSON, genericParseJSON, genericToJSON,  defaultOptions, constructorTagModifier, camelTo2, tagSingleConstructors, sumEncoding, defaultTaggedObject, tagFieldName, GToJSON, Zero)
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

data ErrorOpts = Terminate | LogToConsole | LogToFile | LogToDb deriving (Show, Eq, Generic)

instance FromJSON ErrorOpts where
  parseJSON = genericParseJSON defaultOptions {
                constructorTagModifier = camelTo2 '_' }




data RuntimeException = forall e . (Exception e, ToJSON e) => RuntimeException e 

instance Show RuntimeException where
  show (RuntimeException e) = show e
instance ToJSON RuntimeException where
  toJSON (RuntimeException e) = toJSON e

instance Exception RuntimeException


runtimeExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
runtimeExceptionToException = toException . RuntimeException

runtimeExceptionFromException :: Exception e => SomeException -> Maybe e
runtimeExceptionFromException x = do
  RuntimeException a <- fromException x
  cast a


genericExceptionToJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
genericExceptionToJSON = genericToJSON defaultOptions
  { tagSingleConstructors = True
  , sumEncoding = defaultTaggedObject{ tagFieldName = "error_type"}
  }



data SubjectIdNotFound = SubjectIdNotFound deriving (Generic, Typeable)

instance Show SubjectIdNotFound where
  show _ = "subject_id is required"

instance ToJSON SubjectIdNotFound where
  toJSON = genericExceptionToJSON

instance Exception SubjectIdNotFound where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException



data SheetNotFound = SheetNotFound {name :: T.Text} deriving (Generic, Typeable)

instance Show SheetNotFound where
  show SheetNotFound{..} = "Sheet '" ++ toS name ++ "' does not exist in the current file."

instance ToJSON SheetNotFound where
  toJSON = genericExceptionToJSON

instance Exception SheetNotFound where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException


data CSVParseError = CSVParseError {message :: String} deriving (Generic, Typeable)

instance Show CSVParseError where
  show CSVParseError{..} = message

instance ToJSON CSVParseError where
  toJSON = genericExceptionToJSON

instance Exception CSVParseError where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException



data FileIDCouldNotBefound = FileIDCouldNotBefound {file :: String} deriving (Generic, Typeable)

instance Show FileIDCouldNotBefound where
  show FileIDCouldNotBefound{..} = "File ID for '" ++ (takeFileName file) ++ "' could not be found."

instance ToJSON FileIDCouldNotBefound where
  toJSON = genericExceptionToJSON

instance Exception FileIDCouldNotBefound where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException




data RowError = forall e . (Exception e, ToJSON e) => RowError {row :: Int, _error :: e} 


instance Show RowError where
  show RowError{..} = "Error on line " ++ show row ++ ": " ++ show _error

instance ToJSON RowError where
  toJSON RowError{..} = Object $ HM.fromList [("error_type", String "RowError"), ("row", toJSON row), ("error", toJSON _error)]

instance Exception RowError where
  toException   = runtimeExceptionToException
  fromException = runtimeExceptionFromException




handleError :: (MonadThrow m, MonadIO m) =>
  ErrorOpts -> Maybe Handle -> Maybe (Connection, SourceID, FileID) -> Int -> RuntimeException -> m ()
handleError Terminate _ _ lineNo err = 
  throwM $ RowError lineNo err
handleError LogToConsole _ _ lineNo err = 
  liftIO $ putStrLn $ "Error on line " ++ show lineNo ++ ": " ++ show err
handleError LogToFile (Just logFile) _ lineNo err = 
  liftIO $ hPutStrLn logFile $ "Error in row " ++ show lineNo ++ ": " ++ show err
handleError LogToDb _ (Just (con, srcID, fileID)) lineNo err = liftIO $ do
  putStrLn $ "Error in row " ++ show lineNo ++ ": " ++ show err
  insertError srcID fileID ("Error in row " ++ show lineNo ++ ": " ++ show err) con 
handleError _ _ _ _ _ = undefined




