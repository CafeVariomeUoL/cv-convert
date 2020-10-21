{-# LANGUAGE ExistentialQuantification, UndecidableInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
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
import           Runtime.Utils
import           DB



handleError :: (MonadThrow m, MonadIO m) => TerminateOnError -> 
  ErrorOutput -> Int -> SomeRuntimeException -> Value -> Value -> m ()
handleError (TerminateOnError True) (ErrorOutput ConsoleOutput) lineNo err inp out = do
  throwM $ RowError lineNo err inp out 
handleError (TerminateOnError True) eo lineNo err inp out = do
  handleError (TerminateOnError False) eo lineNo err inp out
  throwM $ RowError lineNo err inp out 
handleError _ (ErrorOutput ConsoleOutput) lineNo err inp out = 
  liftIO $ putStrLn $ (color yellow $ "⚠️  Warning: [error on line " ++ show lineNo ++ "] ") ++ showColor err ++ 
    "\nInput:  " ++ (colorJSON inp) ++ "\nOutput: " ++ (colorJSON out) ++ "\n"
handleError _ (ErrorOutput (JSONFileOutput logFile)   ) lineNo err inp out = 
  liftIO $ hPutStrLn logFile $ show $ RowError lineNo err inp out
handleError _ (ErrorOutput (DBOutput con srcID fileID)) lineNo err inp out = liftIO $ do
  putStrLn $ show $ RowError lineNo err inp out
  insertError srcID fileID (show $ RowError lineNo err inp out) con 
handleError _ _                                   lineNo err inp out = 
  liftIO $ putStrLn $ show $ RowError lineNo err inp out

class ShowColor a where
  showColor :: a -> String

data SomeRuntimeException = forall e . (ShowColor e, Exception e, ToJSON e) => SomeRuntimeException e 

instance Show SomeRuntimeException where
  show (SomeRuntimeException e) = show e
instance ShowColor SomeRuntimeException where
  showColor (SomeRuntimeException e) = showColor e
instance ToJSON SomeRuntimeException where
  toJSON (SomeRuntimeException e) = toJSON e

instance Exception SomeRuntimeException

runtimeExceptionToException :: (ShowColor e, Exception e, ToJSON e) => e -> SomeException
runtimeExceptionToException = toException . SomeRuntimeException

runtimeExceptionFromException :: Exception e => SomeException -> Maybe e
runtimeExceptionFromException x = do
  SomeRuntimeException a <- fromException x
  cast a


newtype RuntimeException e = RuntimeException e 
  deriving Generic
  deriving newtype Show
  deriving newtype ShowColor

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (RuntimeException a) where
  toJSON (RuntimeException a) = genericToJSON opts a
    where
      opts = defaultOptions { tagSingleConstructors = True
                            , sumEncoding = defaultTaggedObject{ tagFieldName = "error_type" }
                            }

instance (Show e, ShowColor e, Typeable e, Generic e, GToJSON Zero (Rep e)) => Exception (RuntimeException e)
  where
    toException = runtimeExceptionToException
    fromException = runtimeExceptionFromException


data SubjectIDNotFound = SubjectIDNotFound Value
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException SubjectIDNotFound)
  deriving Exception via (RuntimeException SubjectIDNotFound)

instance Show SubjectIDNotFound where
  show (SubjectIDNotFound o) = "subject_id is required in " ++ (toS $ encode o)

instance ShowColor SubjectIDNotFound where
  showColor (SubjectIDNotFound o) = "subject_id is required in " ++ colorJSON o


data SheetNotFound = SheetNotFound {name :: T.Text} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException SheetNotFound)
  deriving Exception via (RuntimeException SheetNotFound)

instance Show SheetNotFound where
  show SheetNotFound{..} = "Sheet '" ++ toS name ++ "' does not exist in the current file."

instance ShowColor SheetNotFound where
  showColor SheetNotFound{..} = "Sheet " ++ (color blue $ "'" ++ toS name ++ "'") ++ " does not exist in the current file."


data CSVParseError = CSVParseError {message :: String} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException CSVParseError)
  deriving Exception via (RuntimeException CSVParseError)

instance Show CSVParseError where
  show CSVParseError{..} = message

instance ShowColor CSVParseError where
  showColor CSVParseError{..} = message


data FileIDNotFound = FileIDNotFound {file :: String} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException FileIDNotFound)
  deriving Exception via (RuntimeException FileIDNotFound)

instance Show FileIDNotFound where
  show FileIDNotFound {..} = "File ID for '" ++ takeFileName file ++ "' could not be found."

instance ShowColor FileIDNotFound where
  showColor FileIDNotFound {..} = "File ID for " ++ (color blue $ "'" ++ (takeFileName file) ++ "'") ++ " could not be found."



data RowError = forall e . (ShowColor e, Exception e, ToJSON e) => RowError {row :: Int, _error :: e, input :: Value, output :: Value} 

instance Show RowError where
  show RowError{..} = 
    "Error on line " ++ show row ++ ": " ++ show _error ++ 
    "\nInput:  " ++ (toS $ encode input) ++ "\nOutput: " ++ (toS $ encode output) ++ "\n"


instance ShowColor RowError where
  showColor e = show e

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

instance ShowColor HashMismatch where
  showColor HashMismatch{..} = toS $ "The hash for " <> (color blue $ "'" <> url <> "'") <> " is incorrect.\nExpected: " <> color blue expected_hash <> "\nFound:    " <> color blue found_hash



data RuntimeError = RuntimeError String 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException RuntimeError)
  deriving Exception via (RuntimeException RuntimeError)

instance Show RuntimeError where
  show (RuntimeError e) = e

instance ShowColor RuntimeError where
  showColor e = show e


data JSRuntimeError = JSRuntimeError String 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException JSRuntimeError)
  deriving Exception via (RuntimeException JSRuntimeError)

instance Show JSRuntimeError where
  show (JSRuntimeError e) = e


instance ShowColor JSRuntimeError where
  showColor e = show e
