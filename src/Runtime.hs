{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Runtime
Description : Main entrypoint to cv-convert
Copyright   : (c) Samuel Balco, 2020
License     : MIT
Maintainer  : sam@definitelynotspam.email

The runtime module exports the main functionality of the cv-convert tool.
-}
module Runtime(SourceID, FileType(..), readFileType, SheetName, Settings(..), loadLibrary, processFile) where

import           GHC.Generics
import           Text.Regex.PCRE.Heavy
import           System.IO                    (openFile, IOMode(..), Handle, hClose)
import           Data.Aeson                   (Value(..), FromJSON(..), decode, toJSON, genericParseJSON, defaultOptions, constructorTagModifier)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import qualified Data.ByteString.Lazy         as BS
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad                (when, forM_)
import qualified Data.HashMap.Strict          as HM
import           Control.Monad.Catch          (MonadThrow(..), MonadMask(..), catch)
import           Control.Monad.Reader         (MonadReader)
import           Control.Lens.Combinators     (FoldableWithIndex, ifoldlM)
import           System.FilePath.Posix        (takeFileName, (-<.>))
import           Data.Char                    (toLower)
import           Data.String.Conv             (toS)
import           System.Directory             (doesFileExist)
import qualified Data.HashSet                  as S
import qualified Data.Vector                   as V
import           Database.HDBC.PostgreSQL.Pure(Config(..), Connection, connect)
import           Database.HDBC.Types          (IConnection(commit, disconnect))

import           Runtime.Error
import           Quickjs
import           Quickjs.Error                (InternalError(..))
import           Schema
import           Parse.Xlsx
import           Parse.Csv
import           DB
import           JSON.Utils

data FileType = TXT | JSON | CSV | XLSX deriving (Show, Eq, Generic)

instance FromJSON FileType where
  parseJSON = genericParseJSON defaultOptions{ constructorTagModifier = map toLower }


{-|
Parses a file extension, such as @.txt@ into 'TXT'. 
Returns 'Nothing' if the extension does not mach any of the 'FileType's.
-}
readFileType :: String -> Maybe FileType
readFileType [] = Nothing
readFileType s = decode $ toS $ "\"" ++ (map toLower $ tail s) ++ "\""

newtype SheetName = SheetName {unSheetName :: Text} deriving (Generic, FromJSON, Show, Eq)

data Settings = Settings { 
  processFunction :: String -- ^ A string containg the JS function which will be applied to the input
, jsonSchema :: Maybe Schema -- ^ A JSON schema used for output validation
, openAs :: Maybe FileType -- ^ This parameter can be used to specify the parsing behaviour 
                           -- for files such as @.phenotype@, which should be parsed as 'JSON'.
                           -- If left blank, parsing defaults to either file extension 
                           -- (if it corresponds to one of the 'FileType's), otherwise `TXT`.
, startFrom :: Maybe Int -- ^ This parameter specifies how many rows should be skipped. Only works when parsing TXT files
, onError :: Maybe ErrorOpts -- ^ Specifies the error logging behaviour
, worksheet :: Maybe SheetName -- ^ Used to specify which worksheet should be parsed.
                               -- Only works for 'XLSX' files. If left blank, defaults to first found worksheet.
} deriving (Show, Eq, Generic)
 
instance FromJSON Settings



{-|
The 'loadLibrary' function takse a file path to a JS library and loads it in the current
context. Quickjs does not currently support importing modules in the global mode. 
However, we want to share the library between the react front-end client and this tool,
so we do a bit of hackery to get the lib working for both. As aresult, the library must have the following "shape":

>let Utils = {
>  fun1: function(a) {...},
>  fun2: ...
>}
>export default Utils

Once loaded, the functions are available inside the Quickjs interpreter under @Utils.fun1@, etc.
-}
loadLibrary :: (MonadThrow m, MonadIO m, MonadReader JSContextPtr m) => FilePath -> m ()
loadLibrary libPath = do
  liftIO $ print $ "lib path: " ++ libPath

  libExists <- liftIO $ doesFileExist libPath
  when libExists $ do
    f <- liftIO $ readFile libPath
    -- rather hacky way to remove the keyword default from the lib module
    -- we have to do this because quickjs won't let us import and use JS modules
    let f' = gsub [re|export\s+default|] ("" :: String) f
    _ <- eval_ f'
    liftIO $ print ("loaded lib" :: String)
    return ()
  return ()




{-|
This function runs the converter function on the row and header input data, 
then runs the validator, to check that the JSON 'Value' that was produced validates
agains the JSON schema.

If the output is valid, the data is either stored in a database or a JSON text file.
When inserting into database, the output is stored in two different tables:

  - The 'insertJSONBOverrideOnConflict' function stores the data as Postgres JSONB in the 
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
  -> Either (Connection, SourceID, FileID) Handle -- ^ Either a Database connection or a file handle, 
                                                  -- depending on where we output data
  -> ErrorOpts
  -> Maybe Handle -- ^ Optional handle to a log file, if 'LogToFile' is passed to the 'ErrorOpts' argument
  -> m (Maybe (HM.HashMap Value (S.HashSet Value))) -- ^ returns a map from JSON 'Value's to a set of 'Value's, 
                                                    -- by calling 'createAllPathsWithValues' on the converter function output
convertRow i row header rowFun validator outputHandle onError logFile = do {  
  res <- rowFun row header;
  validate validator res ;
  subjectID <- getSubjectID res ;
  liftIO $ case outputHandle of 
    Left (con, srcID, fileID) -> do
      -- insert record into the JSONB table
      insertJSONBOverrideOnConflict srcID fileID subjectID res con
      -- flatten record into EAV and insert into the EAV table
      (_,eav) <- flattenToEAV res
      forM_ eav $ \(uuid,attr,val) -> insertEAV uuid srcID fileID subjectID attr val con

      return $ Just $ createAllPathsWithValues res
    Right outputFile -> do
      when (i > 0) $ BS.hPutStr outputFile " , "
      BS.hPutStr outputFile $ encodePretty res
      return Nothing ;
} `catch` (\(e::RuntimeException) -> handleError onError logFile maybeDBConn i e >> return Nothing)

  where
    maybeDBConn = case outputHandle of
      Left c -> Just c
      Right _ -> Nothing


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
  -> Maybe Handle
  -> ErrorOpts
  -> Int
  -> Either (Connection, SourceID, FileID) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processTxtFile rowFun validator fName logFile onError startFromLine outputHandle = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  withJSValue ([("h", "data")] :: [(Text.Text, Text.Text)]) $ \header ->
    processRow (Text.lines txt) $ \i l ->
      if (i < startFromLine) then return Nothing
      else withJSValue (Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)]) $ \row ->
        convertRow i row header rowFun validator outputHandle onError logFile
  


processXlsxFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe SheetName
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, SourceID, FileID) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processXlsxFile rowFun validator fName sheetName logFile onError outputHandle = do
  headerRowsMaybe <- readXlsxFile fName (fmap unSheetName sheetName)
  case headerRowsMaybe of
    Just (h, rows) ->
      withJSValue (toJSON h) $ \header ->
        processRow rows $ \i r -> 
          withJSValue r $ \row ->
            convertRow i row header rowFun validator outputHandle onError logFile
    Nothing -> case sheetName of
      Just (SheetName s) -> throwM $ SheetNotFound s
      Nothing -> throwM $ InternalError $ "No sheets found in file."



processCsvFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, SourceID, FileID) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processCsvFile rowFun validator fName logFile onError outputHandle = do
  (h, rows) <- readCsvFile fName
  withJSValue h $ \header ->
    processRow rows $ \i r ->
      withJSValue (Object $ HM.insert "i" (toJSON i) r) $ \row ->
        convertRow i row header rowFun validator outputHandle onError logFile


processJsonFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, SourceID, FileID) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processJsonFile rowFun validator fName logFile onError outputHandle = do 
  f <- liftIO $ BS.readFile fName;
  let 
    (rows, hs) = case decode f of
      Just (Array js) -> (V.toList js, S.toList $ V.foldl (\acc o -> (S.fromList $ getHeader o) `S.union` acc) S.empty js)
      Just o -> ([o], getHeader o)
      Nothing -> ([], [])
  
  withJSValue (Object $ HM.fromList $ map (\h -> ("h", toJSON h)) hs) $ \header ->
    processRow rows $ \i r -> do
      withJSValue (insertI i r) $ \row ->
        convertRow i row header rowFun validator outputHandle onError logFile
   
  where
    getHeader (Object o) = HM.keys o
    getHeader _ = []

    insertI i (Object o) = Object $ HM.insert "i" (toJSON i) o
    insertI i o = Object $ HM.fromList [ ("i",toJSON i), ("data", o) ]


processFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     Maybe Config -- ^ Optional 'Config' parameter for connecting to a DB. If not provided, the function will 
                  -- default to writing the output to a file
  -> Maybe SourceID -- ^ The source_id parameter is only used when writing to a DB
  -> (JSValue -> JSValue -> m Value)  -- ^ Function taking the row and header `JSValue`s, returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> FilePath -- ^ Path of the input file
  -> Maybe SheetName -- ^ Optional sheet name. Only used when 'FileType' is 'XLSX'
  -> FileType
  -> ErrorOpts -- ^ Describes the error behaviour when parsing a row of the input
  -> Int -- ^ Number of rows to skip/row number to start from (only used when 'FileType' is 'TXT')
  -> m ()
processFile dbConnInfo source_id rowFun validator fName sheetName fType onError startFromLine = do
  logFile <- liftIO $ case onError of
    LogToFile -> writeFile (fName -<.> "log") "" >> openFile (fName -<.> "log") AppendMode >>= return . Just
    _ -> return Nothing

  -- if we have db conn info, we will be writing into the db instead of a file
  case (dbConnInfo, source_id) of
    (Just c, Just srcID) -> do
      con <- liftIO $ connect c
      file_id <- liftIO $ getFileID srcID (takeFileName fName) con
      
      _ <- case file_id of
        Just fileID -> do
          -- clear any errors from a possible previous run, if we are logging to db
          case onError of
            LogToDb -> liftIO $ clearErrors srcID fileID con
            _ -> pure ()
          
          jsonAttrVals <- process (Left (con, srcID, fileID)) logFile
          liftIO $ forM_ (HM.toList jsonAttrVals) $ \(attr,vs) ->
            insertJSONBAttributesValuesMergeOnConflict srcID attr (toJSON vs) con
          liftIO $ cleanupJSONBAttributesValues con
        Nothing -> throwM $ FileIDNotFound  fName
      liftIO $ do
        commit con
        disconnect con

    _ -> do
      outputFile <- liftIO $ writeFile (fName -<.> "out.json") "[\n" >> openFile (fName -<.> "out.json") AppendMode
      _ <- process (Right outputFile) logFile
      liftIO $ do
        BS.hPutStr outputFile "\n]"
        hClose outputFile 

  liftIO $ mapM_ hClose logFile
  where 
    process outputHandle logFile = case fType of
      TXT  -> processTxtFile  rowFun validator fName logFile onError startFromLine outputHandle
      XLSX -> processXlsxFile rowFun validator fName sheetName logFile onError outputHandle
      CSV  -> processCsvFile  rowFun validator fName logFile onError outputHandle
      JSON -> processJsonFile rowFun validator fName logFile onError outputHandle
