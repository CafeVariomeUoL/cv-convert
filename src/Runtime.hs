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
module Runtime(SourceID, FileType(..), readFileType, SheetName, LibFunctions(..), Settings(..), loadLibrary, processFile, compileSchema, addOptsToFileType) where

import           GHC.Generics
import           Main.Utf8                    (withUtf8)
import           System.IO                    (openFile, IOMode(..), Handle, hClose)
import           Data.Aeson                   (Value(..), FromJSON(..), decode, toJSON, withText, (.:))
import           Data.Aeson.Types             (unexpected)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as BS

import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad                (when, forM_, unless, mzero)
import qualified Data.HashMap.Strict          as HM
import           Control.Monad.Catch          (MonadThrow(..), MonadMask(..), try, catch, bracket)
import           Control.Monad.Reader         (MonadReader)
import           Control.Lens                 ((^.))
import           Control.Lens.Combinators     (FoldableWithIndex, ifoldlM)
import           System.FilePath.Posix        (takeFileName, (<.>))
import           Data.Char                    (toLower)
import           Data.String.Conv             (toS)
import           System.Directory             (doesFileExist)
import qualified Data.HashSet                 as S
import qualified Data.Vector                  as V
import           Database.HDBC.PostgreSQL.Pure(Config(..), Connection, connect)
import           Database.HDBC.Types          (IConnection(commit, disconnect))
import           Crypto.Hash.SHA256           (hashlazy)
import           Network.Wreq                 (get, responseBody)


import           Runtime.Error
import           Quickjs
import           Quickjs.Error                (InternalError(..))
import           Schema
import           Parse.Xlsx
import           Parse.Csv
import           DB
import           JSON.Utils

data FileType txtOpts jsonOpts csvOpts xlsxOpts = TXT txtOpts | JSON jsonOpts | CSV csvOpts | XLSX xlsxOpts deriving (Eq, Generic)

instance FromJSON (FileType () () () ()) where 
  parseJSON = withText "FileType" $ \case 
    s | s == "txt"  -> return $ TXT ()
    s | s == "json" -> return $ JSON ()
    s | s == "csv"  -> return $ CSV ()
    s | s == "xlsx" -> return $ XLSX ()
    s | otherwise   -> unexpected $ String s

instance Show (FileType a b c d) where 
  show (TXT _)  = "TXT "
  show (JSON _) = "JSON"
  show (CSV _)  = "CSV "
  show (XLSX _) = "XLSX"

{-|
Parses a file extension, such as @.txt@ into 'TXT'. 
Returns 'Nothing' if the extension does not mach any of the 'FileType's.
-}
readFileType :: FilePath -> Maybe (FileType () () () ())
readFileType [] = Nothing
readFileType s = decode $ toS $ "\"" ++ (map toLower $ tail s) ++ "\""


addOptsToFileType :: txtOpts -> jsonOpts -> csvOpts -> xlsxOpts -> FileType a b c d -> FileType txtOpts jsonOpts csvOpts xlsxOpts
addOptsToFileType txtOpts _        _       _        (TXT  _) = TXT txtOpts
addOptsToFileType _       jsonOpts _       _        (JSON _) = JSON jsonOpts
addOptsToFileType _       _        csvOpts _        (CSV  _) = CSV csvOpts
addOptsToFileType _       _        _       xlsxOpts (XLSX _) = XLSX xlsxOpts

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


data Settings = Settings { 
  processFunction :: Text -- ^ A string containg the JS function which will be applied to the input
, libraryFunctions :: Maybe LibFunctions
, jsonSchema :: Maybe Schema -- ^ A JSON schema used for output validation
, openAs :: Maybe (FileType () () () ()) -- ^ This parameter can be used to specify the parsing behaviour 
                           -- for files such as @.phenotype@, which should be parsed as 'JSON'.
                           -- If left blank, parsing defaults to either file extension 
                           -- (if it corresponds to one of the 'FileType's), otherwise `TXT`.
, startFrom :: Maybe Int -- ^ This parameter specifies how many rows should be skipped. Only works when parsing TXT files
, onError :: Maybe (ErrorOpts () ()) -- ^ Specifies the error logging behaviour
, worksheet :: Maybe SheetName -- ^ Used to specify which worksheet should be parsed.
                               -- Only works for 'XLSX' files. If left blank, defaults to first found worksheet.
} deriving (Show, Eq, Generic)

instance FromJSON Settings


data DataOutput = JSONFileOutput Handle 
                | DBOutput { 
                    con :: Connection
                  , sourceID :: SourceID
                  , fileID :: FileID
                  }

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
  -> ErrorOpts Handle (Connection, SourceID, FileID)
  -> Value -- ^ Row data as JSON
  -> m (Maybe (HM.HashMap Value (S.HashSet Value))) -- ^ returns a map from JSON 'Value's to a set of 'Value's, 
                                                    -- by calling 'createAllPathsWithValues' on the converter function output
convertRow i row header rowFun validator outputHandle onError rowJSON = 
  try (rowFun row header) >>= \case
    Left (e::SomeRuntimeException) -> handleError onError i e rowJSON "" >> return Nothing
    Right res -> do {
      validate validator res ;
      subjectID <- getSubjectID res ;
      liftIO $ case outputHandle of 
        DBOutput{..} -> do
          -- insert record into the JSONB table
          insertJSONBOverwriteOnConflict sourceID fileID subjectID res con
          -- flatten record into EAV and insert into the EAV table
          (_,eav) <- flattenToEAV res
          forM_ eav $ \(uuid,attr,val) -> insertEAV uuid sourceID fileID subjectID attr val con

          return $ Just $ createAllPathsWithValues res
        JSONFileOutput outputFileHandle -> do
          when (i > 0) $ BSL.hPutStr outputFileHandle " , "
          BSL.hPutStr outputFileHandle $ encodePretty res
          return Nothing ;
    } `catch` \(e::SomeRuntimeException) -> handleError onError i e rowJSON res >> return Nothing

{-|
Helper function which loops over the rows of the given input, 
collecting and merging all the resulting maps (generated inside 'convertRow' via 'createAllPathsWithValues') 
-}
processRow :: (FoldableWithIndex Int f, MonadThrow m) => 
       f row -- ^ Any list like data structure containing rows we can loop over with an index
    -> (Int -> row -> m (Maybe (HM.HashMap Value (S.HashSet Value)))) -- ^ Function that consumes the row together with it's index
    -> m (HM.HashMap Value (S.HashSet Value))
processRow input m = ifoldlM (\i jsonAttrValsAcc l -> do
    jsonAttrValsMaybe <- m i l
    case jsonAttrValsMaybe of 
      Just jsonAttrVals -> return $ HM.unionWith (S.union) jsonAttrVals jsonAttrValsAcc
      Nothing -> return jsonAttrValsAcc
  ) HM.empty input


processTxtFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpts Handle (Connection, SourceID, FileID)
  -> Int
  -> m (HM.HashMap Value (S.HashSet Value))
processTxtFile rowFun validator fName outputHandle onError startFromLine = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  withJSValue ([("h", "data")] :: [(Text.Text, Text.Text)]) $ \header ->
    processRow (Text.lines txt) $ \i l ->
      if (i < startFromLine) then return Nothing
      else let rowJSON = Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)] in withJSValue rowJSON $ \row ->
        convertRow i row header rowFun validator outputHandle onError rowJSON
  


processXlsxFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpts Handle (Connection, SourceID, FileID)
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
      Nothing -> throwM $ InternalError $ "No sheets found in file."



processCsvFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOpts Handle (Connection, SourceID, FileID)
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
  -> ErrorOpts Handle (Connection, SourceID, FileID)
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



-- -- processFile :: (
-- --  MonadMask m, MonadIO m
-- HasReader "onOutput" OutputOpts m
-- HasReader "onError" ErrorOpts m

-- -- ) =>
--      (JSValue -> JSValue -> m Value)  -- ^ Function taking the row and header `JSValue`s, returning a JSON 'Value'
--   -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
--                                    -- conforms to the JSON schema, specified in 'jsonSchema'
--   -> FilePath -- ^ Path of the input file
--   -> FileType ? ? ? ?
--   -> m ()


processFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     Maybe (Config, SourceID) -- ^ Optional 'Config' and 'SourceID' parameter for connecting to a DB. If not provided, the function will 
                              -- default to writing the output and logs to disk
  -> (JSValue -> JSValue -> m Value)  -- ^ Function taking the row and header `JSValue`s, returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> FilePath -- ^ Path of the input file
  -> FileType Int () () (Maybe SheetName)  -- ^ Number of rows to skip/row number to start from only used when 'FileType' is 'TXT'. Default is 0.
                                           -- Optional sheet name only used when 'FileType' is 'XLSX'.
  -> ErrorOpts () () -- ^ Describes the error behaviour when parsing a row of the input. 
  -> m ()
processFile dbConnInfo rowFun validator fName fType onError = 
  bracket
    -- open a log file if 'LogToFile' was passed in onError
    (withLogToFileErrorOpt onError (\_ -> 
        liftIO $ writeFile (fName <.> "log") "" >> openFile (fName <.> "log") AppendMode))
    -- close the log file after we processed the file
    (\case
      LogToFile logFile -> liftIO $ hClose logFile
      _ -> pure ()) $
    -- main processing function
    \(onErrorWLogFile :: ErrorOpts Handle ()) ->
      -- if we have db conn info, we will be writing into the db instead of a file
      case dbConnInfo of
        Just (c, sourceID) ->
          bracket
            -- open a connection to the db
            (liftIO $ connect c) 
            -- this action runs after completing the main body function
            (\con -> liftIO $ commit con >> disconnect con) $ 
            -- main body function
            \con -> do 
              fileID <- do
                liftIO $ getFileID sourceID (takeFileName fName) con >>= \case
                  Just fileID -> do
                    -- clear any errors from a possible previous run, if we are logging to db
                    withLogToDBErrorOpt_ onError $ liftIO $ clearErrors sourceID fileID con
                    return fileID
                  Nothing -> throwM $ FileIDNotFound fName

              jsonAttrVals <- withLogToDBErrorOpt onErrorWLogFile (const $ return (con, sourceID, fileID)) >>= process DBOutput{..}
              _ <- liftIO $ do
                forM_ (HM.toList jsonAttrVals) $ \(attr,vs) ->
                  insertJSONBAttributesValuesMergeOnConflict sourceID attr (toJSON vs) con

                cleanupJSONBAttributesValues con
              
              pure ()

        _ -> 
          bracket
            -- open an output file and write an opening bracket '['
            (liftIO $ writeFile (fName <.> "out.json") "[\n" >> openFile (fName <.> "out.json") AppendMode)
            -- after the main body, write closing bracket ']' and close the file
            (\outputFileHandle -> liftIO $ BSL.hPutStr outputFileHandle "\n]" >> hClose outputFileHandle) $
            -- main body
            \outputFileHandle ->
              -- we have to lift 'onErrorWLogFile' from 
              --   ErrorOpts Handle () 
              -- to 
              --   ErrorOpts Handle (Connection, SourceID, FileID)
              -- in order to pass it to processTxtFile/processXlsxFile/etc.
              -- however, we are not actually expecting 'onErrorWLogFile' to have the value 'LogToDb ()'
              -- since no DB Config + source_id info has been passed to this function therefore
              -- if we do encounter this value, something went wrong and we should throw an error.
              withLogToDBErrorOpt onErrorWLogFile 
                (const $ throwM $ InternalError "trying to log to DB without providing a DB connection.") >>= 
              process (JSONFileOutput outputFileHandle) >>
              return ()   
  where
    process outputHandle onErrorWLogAndDB = case fType of
      TXT startFromLine -> processTxtFile  rowFun validator fName outputHandle onErrorWLogAndDB startFromLine 
      XLSX sheetName    -> processXlsxFile rowFun validator fName outputHandle onErrorWLogAndDB sheetName
      CSV _             -> processCsvFile  rowFun validator fName outputHandle onErrorWLogAndDB
      JSON _            -> processJsonFile rowFun validator fName outputHandle onErrorWLogAndDB
