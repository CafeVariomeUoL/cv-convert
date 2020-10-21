{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module Runtime.Types where
import           Data.Aeson               (Value(..), FromJSON(..), (.:), (.:?))
import           Data.Aeson.Types         (Parser, unexpected)
import           Data.Text                (Text)
import           Data.String.Conv         (toS)
import           Control.Monad            (join, MonadPlus(mzero))
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)
import qualified Data.ByteString          as BS
import           JSONSchema.Draft4.Schema (Schema)
import           System.IO                (Handle)

import DB ( FileID, SourceID, ConnInfo, DBConn )


{-|
Type encoding the input document type along with potential parsing/processing options.
-}
data FileType = TXTFile {startFromLine :: Int} 
              | JSONFile
              | CSVFile
              | XLSXFile {sheetName :: Maybe SheetName} deriving (Eq, Generic)


data Specified a = Specified a | Inferred a | Unspecified deriving (Show, Eq)



instance Show FileType where 
  show TXTFile{..}  = "TXT"
  show JSONFile = "JSON"
  show CSVFile  = "CSV"
  show XLSXFile{..} = "XLSX"

{-|
Parses a file extension, such as @.txt@ into 'TXTFile'. 
Returns 'Nothing' if the extension does not mach any of the 'FileType's.
-}
-- readFileType :: Maybe Int -> Maybe SheetName -> Maybe String -> FileType
parseFileType :: Maybe Int -> Maybe SheetName -> Maybe Text -> Parser (Specified FileType)
parseFileType startFrom sheetName s = case s of
  Just "txt"  -> return $ Specified $ TXTFile (fromMaybe 0 startFrom)
  Just "json" -> return $ Specified $ JSONFile
  Just "csv"  -> return $ Specified $ CSVFile
  Just "xlsx" -> return $ Specified $ XLSXFile sheetName
  Just unknown -> unexpected $ String unknown
  Nothing      -> case (startFrom, sheetName) of
    (Just _, Nothing) -> return $ Inferred $ TXTFile (fromMaybe 0 startFrom)
    (Nothing, Just _) -> return $ Inferred $ XLSXFile sheetName
    _                 -> return Unspecified



fromSpecifiedFileType :: String -> Specified FileType -> FileType
fromSpecifiedFileType _      (Specified fileType)             = fileType
fromSpecifiedFileType "xlsx" (Inferred fileType@(XLSXFile _)) = fileType
fromSpecifiedFileType "txt"  (Inferred fileType@(TXTFile _))  = fileType
fromSpecifiedFileType "json" _                                = JSONFile
fromSpecifiedFileType "csv"  _                                = CSVFile
fromSpecifiedFileType "xlsx" _                                = XLSXFile Nothing
fromSpecifiedFileType _      _                                = TXTFile 0


{-|
Data type used to describe the error reporting/handling behaviour when processing a row. 
Other than 'Terminate, all other options result in the error from a row being recorded/printed,
with the computation continuing onto the next row.
-}
data ErrorOpt = LogToConsole | LogToFile | LogToDB deriving (Show, Eq, Generic)


-- instance FromJSON ErrorOpt where 
--   parseJSON = withText "ErrorOpt" $ \case 
--     s | s == "terminate"  -> return Terminate
--     s | s == "log_to_console" -> return LogToConsole
--     s | s == "log_to_file" -> return LogToFile
--     s | s == "log"  -> return $ Log
--     s | otherwise   -> unexpected $ String s




newtype SheetName = SheetName {unSheetName :: Text} 
  deriving stock   (Show, Eq, Generic)
  deriving newtype FromJSON -- we need to derive this as newtype, so the JSON parsing treats SheetName same as just Text


data LibFunctions = Inline BS.ByteString
                  | External {
                      url :: Text
                    , hash :: BS.ByteString
                    } deriving (Show, Eq, Generic)

instance FromJSON LibFunctions where
  parseJSON (Object v) =
    External <$> v .: "url"
            <*> ((toS :: Text -> BS.ByteString) <$> (v .: "hash"))
  parseJSON (String t) = return $ Inline $ toS t
  parseJSON _ = mzero

{-|
Internal representation of a '.settings' file.
-}
data Settings = Settings { 
  processFunction :: Text -- ^ A string containg the JS function which will be applied to the input
, libraryFunctions :: Maybe LibFunctions
, jsonSchema :: Maybe Schema -- ^ A JSON schema used for output validation
, openAs :: Specified FileType -- ^ This parameter can be used to specify the parsing behaviour 
                           -- for files such as @.phenotype@, which should be parsed as 'JSON'.
                           -- If left blank, parsing defaults to either file extension 
                           -- (if it corresponds to one of the 'FileType's), otherwise 'TXTFile'.
-- , startFrom :: Maybe Int -- ^ This parameter specifies how many rows should be skipped. Only works when parsing TXTFile files
-- , onError :: Maybe ErrorOpt -- ^ Specifies the error logging behaviour
-- , worksheet :: Maybe SheetName -- ^ Used to specify which worksheet should be parsed.
                               -- Only works for 'XLSXFile' files. If left blank, defaults to first found worksheet.
} deriving (Show, Eq, Generic)

instance FromJSON Settings where
  parseJSON (Object v) = Settings
      <$> v .:  "processFunction"
      <*> v .:? "libraryFunctions"
      <*> v .:? "jsonSchema"
      <*> (join $ parseFileType <$> v .:? "startFrom" <*> v .:? "worksheet" <*> v .:? "openAs")
      -- <*> v .:? "onError"
  parseJSON o = unexpected o

{-|
Defines the output behaviour, either writing to a JSON/SQL file or a database. 
-}
data DataOutputOpt = JSONFileOutputOpt
                   | DBOutputOpt ConnInfo SourceID -- ^ If writing to a database, the `ConnInfo` and `SourceID` paramameters must be supplied
                   | SQLFileOutputOpt SourceID -- ^ If writing to an SQL file, the `SourceID` paramameter must be supplied

{-|
Sum type for the handles for different output options. 
-}
data DataOutput = JSONFileOutput Handle -- ^ `Handle` points to an open JSON file
                | forall con_t. (DBConn con_t) => DBOutput { 
                    con :: con_t -- ^ Database connection handle, depends on the backend (MySQL/Postgres).
                  , sourceID :: SourceID
                  , fileID :: FileID
                  }
                | SQLFileOutput SourceID Handle -- ^ `Handle` points to an open SQL file
                | ConsoleOutput

newtype ErrorOutput = ErrorOutput DataOutput


newtype TerminateOnError = TerminateOnError Bool