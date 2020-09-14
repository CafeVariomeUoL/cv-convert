{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Runtime
Description : Main entrypoint to cv-convert
Copyright   : (c) Samuel Balco, 2020
License     : MIT
Maintainer  : sam@definitelynotspam.email

The runtime module exports the main functionality of the cv-convert tool.
-}
module Runtime(SourceID, OutputOpt(..), FileType(..), readFileType, SheetName, LibFunctions(..), Settings(..), loadLibrary, processFile, compileSchema, addOptsToFileType) where

import           GHC.Generics                  (Generic)
import           Main.Utf8                     (withUtf8)
import           System.IO                     (openFile, IOMode(..), Handle, hClose)
import           Data.Aeson                    (Value(..), FromJSON(..), decode, toJSON, withText, (.:))
import           Data.Aeson.Types              (unexpected)
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BS

import           Data.Bitraversable            (bimapM)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad                 (when, forM_, unless, mzero)
import qualified Data.HashMap.Strict           as HM
import           Control.Monad.Catch           (MonadThrow(..), MonadMask(..), try, catch, bracket)
import           Control.Monad.Reader          (MonadReader)
import           Control.Lens                  ((^.))
import           Control.Lens.Combinators      (FoldableWithIndex, ifoldlM)
import           System.FilePath.Posix         (takeFileName, (<.>))
import           Data.Char                     (toLower)
import           Data.String.Conv              (toS)
import           System.Directory              (doesFileExist)
import qualified Data.HashSet                  as S
import qualified Data.Vector                   as V
import           Database.HDBC.PostgreSQL.Pure as Postgres
import           Database.MySQL.Base           as MySQL
import           Database.HDBC.Types           (IConnection(commit, disconnect))
import           Crypto.Hash.SHA256            (hashlazy)
import           Network.Wreq                  (get, responseBody)


import           Runtime.Error
import           Quickjs                      (JSValue, JSContextPtr, eval_, withJSValue)
import           Schema                       (compileSchema, validate, ValidatorFailure, Schema)
import           Parse.Xlsx                   (readXlsxFile)
import           Parse.Csv                    (readCsvFile)
import           DB
import qualified DB.Postgres                  as Postgres
import qualified DB.MySQL                     as MySQL
import           JSON.Utils                   (createAllPathsWithValues, flattenToEAV, getSubjectID)


{-|
Type encoding the input document type along with potential parsing/processing options.
-}
data FileType txtOpts jsonOpts csvOpts xlsxOpts = TXTFile txtOpts | JSONFile jsonOpts | CSVFile csvOpts | XLSXFile xlsxOpts deriving (Eq, Generic)

instance FromJSON (FileType () () () ()) where 
  parseJSON = withText "FileType" $ \case 
    s | s == "txt"  -> return $ TXTFile ()
    s | s == "json" -> return $ JSONFile ()
    s | s == "csv"  -> return $ CSVFile ()
    s | s == "xlsx" -> return $ XLSXFile ()
    s | otherwise   -> unexpected $ String s

instance Show (FileType a b c d) where 
  show (TXTFile _)  = "TXT"
  show (JSONFile _) = "JSON"
  show (CSVFile _)  = "CSV"
  show (XLSXFile _) = "XLSX"

{-|
Parses a file extension, such as @.txt@ into 'TXTFile'. 
Returns 'Nothing' if the extension does not mach any of the 'FileType's.
-}
readFileType :: FilePath -> Maybe (FileType () () () ())
readFileType [] = Nothing
readFileType s = decode $ toS $ "\"" ++ (map toLower $ tail s) ++ "\""


addOptsToFileType :: txtOpts -> jsonOpts -> csvOpts -> xlsxOpts -> FileType a b c d -> FileType txtOpts jsonOpts csvOpts xlsxOpts
addOptsToFileType txtOpts _        _       _        (TXTFile  _) = TXTFile txtOpts
addOptsToFileType _       jsonOpts _       _        (JSONFile _) = JSONFile jsonOpts
addOptsToFileType _       _        csvOpts _        (CSVFile  _) = CSVFile csvOpts
addOptsToFileType _       _        _       xlsxOpts (XLSXFile _) = XLSXFile xlsxOpts

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
, openAs :: Maybe (FileType () () () ()) -- ^ This parameter can be used to specify the parsing behaviour 
                           -- for files such as @.phenotype@, which should be parsed as 'JSON'.
                           -- If left blank, parsing defaults to either file extension 
                           -- (if it corresponds to one of the 'FileType's), otherwise 'TXTFile'.
, startFrom :: Maybe Int -- ^ This parameter specifies how many rows should be skipped. Only works when parsing TXTFile files
, onError :: Maybe (ErrorOpt () ()) -- ^ Specifies the error logging behaviour
, worksheet :: Maybe SheetName -- ^ Used to specify which worksheet should be parsed.
                               -- Only works for 'XLSXFile' files. If left blank, defaults to first found worksheet.
} deriving (Show, Eq, Generic)

instance FromJSON Settings

{-|
Output options specified by the '-o' flag via the CLI (options are '-o db|sql|json')
-}
data OutputOpt = DB | SQL | JSON deriving (Show, Eq, Generic)


data DataOutput = JSONFileOutput Handle 
                | DBOutput { 
                    con :: DBConn
                  , sourceID :: SourceID
                  , fileID :: FileID
                  }
                | SQLFileOutput SourceID Handle

{-|
The 'loadLibrary' function takse a 'LibFunctions' parameter, which either contains an inline JS script or points
to an external script via URL. The library functions must be stored in a specific way, namely the library file must
be of the form

>Lib.fun1 = (a,b) => {...},
>Lib.fun2 = ...
-}
loadLibrary :: (MonadThrow m, MonadIO m, MonadReader JSContextPtr m) => LibFunctions -> m ()
loadLibrary (Inline script) = eval_ ("Lib = {};\n" <> script) >> return ()
loadLibrary External{..} = do
  let libPath = (toS hash) <.> "js"
  lib <- liftIO $ doesFileExist libPath >>= \case
    True -> liftIO $ withUtf8 $ BS.readFile libPath
    False -> do
      r <- liftIO $ get $ toS url
      -- get the contents (as a lazy ByteString)
      let contents = r ^. responseBody
          contents_hash = toS $ BS.toLazyByteString $ BS.byteStringHex $ hashlazy contents

      unless (contents_hash == hash) $ throwM $ HashMismatch {
          url = url
        , expected_hash = toS hash
        , found_hash = toS contents_hash
        }
      liftIO $ BSL.writeFile libPath contents
      return $ toS contents

  _ <- eval_ $ "Lib = {};\n" <> lib
  return ()



{-|
This function runs the converter function on the row and header input data, 
then runs the validator(to check that the JSON 'Value' that was produced validates
agains the JSON schema).

If the output is valid, the data is either stored in a database or a JSON text file.
When inserting into database, the output is stored in two different tables:

  - The 'insertJSONBOverwriteOnConflict' function stores the data as Postgres JSONB in the 
    @eavs_jsonb_attributes_values@ table
  - 'flattenToEAV' is used to generate flattened EAV triples form the given JOSn object, which are
    then stored in the @eavs@ table via 'insertEAV'

Finally, we return result of running 'createAllPathsWithValues' on the JSON output data.
-}
convertRow :: (MonadMask m, MonadIO m) =>
     Int -- ^ Row index/counter
  -> row -- ^ Row data
  -> header -- ^ Row header
  -> (row -> header -> m Value) -- ^ Coverter function taking the row and header returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> DataOutput -- ^ Either a Database connection or a file handle, 
                -- depending on where we output data
  -> ErrorOpt Handle (DBConn, SourceID, FileID)
  -> Value -- ^ Row data as JSON
  -> m (HM.HashMap Value (S.HashSet Value)) -- ^ returns a map from JSON 'Value's to a set of 'Value's, 
                                                    -- by calling 'createAllPathsWithValues' on the converter function output
convertRow i row header rowFun validator outputHandle onError rowJSON = 
  try (rowFun row header) >>= \case
    -- Below, we catch in two stages, to get better errors. Notice the second catch in 
    -- the Right branch has the additional parameter res, which is added to the error
    -- output for easier debugging.
    Left (e::SomeRuntimeException) -> handleError onError i e rowJSON "" >> return HM.empty
    Right (Array resMultiple) -> 
      -- if rowFun returns an array of records, we process each one and merge the resulting
      -- jsonAttrVals from each run.
      ifoldlM (\j jsonAttrValsAcc res -> 
        do
          jsonAttrVals <- process j res
          return $ HM.unionWith (S.union) jsonAttrVals jsonAttrValsAcc
        ) HM.empty resMultiple 
    Right res -> process (0::Int) res
  where
    process j res = do {
      validate validator res ;
      subjectID <- getSubjectID res ;
      liftIO $ case outputHandle of 
        DBOutput{..} -> do
          -- insert record into the JSONB table
          case con of 
            Left conPostgres -> Postgres.insertJSONBOverwriteOnConflict sourceID fileID subjectID res conPostgres
            Right _ -> pure ()
          -- flatten record into EAV and insert into the EAV table
          (_,eav) <- flattenToEAV res
          forM_ eav $ \(uuid,attr,val) -> insertEAV uuid sourceID fileID subjectID attr val con
          return $ createAllPathsWithValues res
        JSONFileOutput outputFileHandle -> do
          when (i > 0 || j > 0) $ BSL.hPutStr outputFileHandle " , "
          BSL.hPutStr outputFileHandle $ encodePretty res
          return HM.empty ;
        SQLFileOutput sourceID outputFileHandle -> do
          (_,eav) <- flattenToEAV res
          forM_ eav $ \(uuid,attr,val) -> BSL.hPutStr outputFileHandle $ toS $ (fromQuery $ MySQL.insertEAVPrepareQuery uuid sourceID (FileID 0) subjectID attr val) <> ";\n"
          return HM.empty ;
    } `catch` \(e::SomeRuntimeException) -> handleError onError i e rowJSON res >> return HM.empty

{-|
Helper function which loops over the rows of the given input, 
collecting and merging all the resulting maps (generated inside 'convertRow' via 'createAllPathsWithValues') 
-}
processRow :: (FoldableWithIndex Int f, MonadThrow m) => 
       f row -- ^ Any list like data structure containing rows we can loop over with an index
    -> (Int -> row -> m (HM.HashMap Value (S.HashSet Value))) -- ^ Function that consumes the row together with it's index
    -> m (HM.HashMap Value (S.HashSet Value))
processRow input m = ifoldlM (\i jsonAttrValsAcc l -> do
    jsonAttrVals <- m i l
    return $ HM.unionWith (S.union) jsonAttrVals jsonAttrValsAcc
  ) HM.empty input


processTxtFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpt Handle (DBConn, SourceID, FileID)
  -> Int
  -> m (HM.HashMap Value (S.HashSet Value))
processTxtFile rowFun validator fName outputHandle onError startFromLine = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  withJSValue ([("h", "data")] :: [(Text.Text, Text.Text)]) $ \header ->
    processRow (Text.lines txt) $ \i l ->
      if (i < startFromLine) then return HM.empty
      else let rowJSON = Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)] in withJSValue rowJSON $ \row ->
        convertRow i row header rowFun validator outputHandle onError rowJSON
  


processXlsxFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpt Handle (DBConn, SourceID, FileID)
  -> Maybe SheetName
  -> m (HM.HashMap Value (S.HashSet Value))
processXlsxFile rowFun validator fName outputHandle onError sheetName = do
  headerRowsMaybe <- readXlsxFile fName (fmap unSheetName sheetName)
  case headerRowsMaybe of
    Just (h, rows) ->
      withJSValue (toJSON h) $ \header ->
        processRow rows $ \i r -> 
          withJSValue r $ \row ->
            convertRow i row header rowFun validator outputHandle onError r
    Nothing -> case sheetName of
      Just (SheetName s) -> throwM $ SheetNotFound s
      Nothing -> throwM $ RuntimeError $ "No sheets found in file."



processCsvFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpt Handle (DBConn, SourceID, FileID)
  -> m (HM.HashMap Value (S.HashSet Value))
processCsvFile rowFun validator fName outputHandle onError = do
  (h, rows) <- readCsvFile fName
  withJSValue h $ \header ->
    processRow rows $ \i r ->
      let rowJSON = Object $ HM.insert "i" (toJSON i) r in withJSValue rowJSON $ \row ->
        convertRow i row header rowFun validator outputHandle onError rowJSON


processJsonFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpt Handle (DBConn, SourceID, FileID)
  -> m (HM.HashMap Value (S.HashSet Value))
processJsonFile rowFun validator fName outputHandle onError = do 
  f <- liftIO $ BSL.readFile fName;
  let 
    (rows, hs) = case decode f of
      Just (Array js) -> (V.toList js, S.toList $ V.foldl' (\acc o -> (S.fromList $ getHeader o) `S.union` acc) S.empty js)
      Just o -> ([o], getHeader o)
      Nothing -> ([], [])
  
  withJSValue (Object $ HM.fromList $ map (\h -> ("h", toJSON h)) hs) $ \header ->
    processRow rows $ \i r -> do
      let rowJSON = insertI i r in withJSValue rowJSON $ \row ->
        convertRow i row header rowFun validator outputHandle onError rowJSON
   
  where
    getHeader (Object o) = HM.keys o
    getHeader _ = []

    insertI i (Object o) = Object $ HM.insert "i" (toJSON i) o
    insertI i o = Object $ HM.fromList [ ("i",toJSON i), ("data", o) ]





processFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     Maybe (Either Postgres.Config MySQL.ConnectInfo) -- ^ Optional 'Config'/'ConnectInfo' (PostreSQL/MySQL) If not provided, the function will 
                              -- default to writing the output and logs to disk
  -> Maybe SourceID -- ^ Optional 'SourceID' parameter for connecting to a DB or doing a DB dump. 
  -> OutputOpt
  -> (JSValue -> JSValue -> m Value)  -- ^ Function taking the row and header `JSValue`s, returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> FilePath -- ^ Path of the input file
  -> FileType Int () () (Maybe SheetName)  -- ^ Number of rows to skip/row number to start from only used when 'FileType' is 'TXTFile'. Default is 0.
                                           -- Optional sheet name only used when 'FileType' is 'XLSXFile'.
  -> ErrorOpt () () -- ^ Describes the error behaviour when parsing a row of the input. 
  -> m ()
processFile dbConnInfo srcID outOpt rowFun validator fName fType onError = 
  bracket
    -- open a log file if 'LogToFile' was passed in onError
    (withLogToFileErrorOpt onError (\_ -> 
        liftIO $ writeFile (fName <.> "log") "" >> openFile (fName <.> "log") AppendMode))
    -- close the log file after we processed the file
    (\case
      LogToFile logFile -> liftIO $ hClose logFile
      _ -> pure ()) $
    -- main processing function
    \(onErrorWLogFile :: ErrorOpt Handle ()) ->
      -- if we have db conn info, we will be writing into the db instead of a file
      case (outOpt, dbConnInfo, srcID) of
        -- Postgres
        (DB, Just c, Just sourceID) ->
          bracket
            -- open a connection to the db
            (liftIO $ bimapM Postgres.connect MySQL.connect c) 
            -- this action runs after completing the main body function
            (liftIO . either (\con -> commit con >> disconnect con) MySQL.close) $ 
            -- main body function
            \(con :: Either Postgres.Connection MySQL.MySQLConn) -> do 
              fileID <- do
                liftIO $ getFileID sourceID (takeFileName fName) con >>= \case
                  Just fileID -> do
                    -- clear any errors from a possible previous run, if we are logging to db
                    withLogToDBErrorOpt_ onError $ liftIO $ clearErrors sourceID fileID con
                    return fileID
                  Nothing -> throwM $ FileIDNotFound fName
              jsonAttrVals <- withLogToDBErrorOpt onErrorWLogFile (const $ return (con, sourceID, fileID)) >>= process DBOutput{..}
              liftIO $ case con of 
                Left conPostgres -> do
                  forM_ (HM.toList jsonAttrVals) $ \(attr,vs) ->
                    Postgres.insertJSONBAttributesValuesMergeOnConflict sourceID attr (toJSON vs) conPostgres
                  _ <- Postgres.cleanupJSONBAttributesValues conPostgres
                  return ()
                Right _-> pure ()
        (JSON, _, _) -> 
          bracket
            -- open an output file and write an opening bracket '['
            (liftIO $ writeFile (fName <.> "out.json") "[\n" >> openFile (fName <.> "out.json") AppendMode)
            -- after the main body, write closing bracket ']' and close the file
            (\outputFileHandle -> liftIO $ BSL.hPutStr outputFileHandle "\n]" >> hClose outputFileHandle) $
            -- main body
            \outputFileHandle ->
              -- we have to lift 'onErrorWLogFile' from 
              --   ErrorOpt Handle () 
              -- to 
              --   ErrorOpt Handle (Connection, SourceID, FileID)
              -- in order to pass it to processTxtFile/processXlsxFile/etc.
              -- however, we are not actually expecting 'onErrorWLogFile' to have the value 'LogToDb ()'
              -- since no DB Config + source_id info has been passed to this function therefore
              -- if we do encounter this value, something went wrong and we should throw an error.
              withLogToDBErrorOpt onErrorWLogFile 
                (const $ throwM $ RuntimeError "Trying to log to DB without providing a DB connection.") >>= 
              process (JSONFileOutput outputFileHandle) >>
              return ()
        (SQL, _, Just sourceID) -> 
          bracket
            -- open an output file 
            (liftIO $ openFile (fName <.> "out.sql") AppendMode)
            -- after the main body, close the file
            (\outputFileHandle -> liftIO $ hClose outputFileHandle) $
            -- main body
            \outputFileHandle ->
              withLogToDBErrorOpt onErrorWLogFile 
                (const $ throwM $ RuntimeError "Trying to log to DB without providing a DB connection.") >>= 
              process (SQLFileOutput sourceID outputFileHandle) >>
              return ()
        (SQL, _, Nothing) -> do
          throwM $ RuntimeError "Please specify a souce id via --source_id, when output is Postgres/MySQL."
  where
    process outputHandle onErrorWLogAndDB = case fType of
      TXTFile startFromLine -> processTxtFile  rowFun validator fName outputHandle onErrorWLogAndDB startFromLine 
      XLSXFile sheetName    -> processXlsxFile rowFun validator fName outputHandle onErrorWLogAndDB sheetName
      CSVFile _             -> processCsvFile  rowFun validator fName outputHandle onErrorWLogAndDB
      JSONFile _            -> processJsonFile rowFun validator fName outputHandle onErrorWLogAndDB
