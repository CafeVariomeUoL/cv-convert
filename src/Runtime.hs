{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
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
module Runtime(SourceID, DataOutputOpt(..), ErrorOpt(..), TerminateOnError(..), WriteCountToDB(..), FileType(..), SheetName, LibFunctions(..), Settings(..), loadLibrary, fromSpecifiedFileType, processFile, compileSchema) where

import           Main.Utf8                (withUtf8)
import           System.IO                (openFile, IOMode(..), hClose)
import           Data.Aeson               (Value(..), decode, toJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Builder  as BS
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad            (when, forM_, unless)
import qualified Data.HashMap.Strict      as HM
import           Control.Monad.Catch      (MonadThrow(..), MonadMask(..), try, catch, bracket)
import           Control.Monad.Reader     (MonadReader)
import           System.FilePath.Posix    (takeFileName, (<.>))
import           Data.String.Conv         (toS)
import           System.Directory         (doesFileExist)
import qualified Data.HashSet             as S
import qualified Data.Vector              as V
import           Database.MySQL.Base      as MySQL
import           Crypto.Hash.SHA256       (hashlazy)
import           Network.HTTP             (simpleHTTP, mkRequest, RequestMethod(GET), getResponseBody)
import           Network.URI              (parseURI)

import           Runtime.Types
import           Runtime.Utils
import           Runtime.Error
import           Quickjs                  (JSValue, JSContextPtr, eval_, withJSValue)
import           Quickjs.Error            (SomeJSRuntimeException)
import           Schema                   (compileSchema, validate, ValidatorFailure)
import           Parse.Xlsx               (readXlsxFile)
import           Parse.Csv                (readCsvFile)
import           DB
import qualified DB.Postgres              as Postgres
import qualified DB.MySQL                 as MySQL
import           JSON.Utils               (createAllPathsWithValues, flattenToEAV, getSubjectID)
import Control.Monad (void)



{-|
The 'loadLibrary' function takse a 'LibFunctions' parameter, which either contains an inline JS script or points
to an external script via URL. The library functions must be stored in a specific way, namely the library file must
be of the form

>Lib.fun1 = (a,b) => {...},
>Lib.fun2 = ...
-}
loadLibrary :: (MonadThrow m, MonadIO m, MonadReader JSContextPtr m) => LibFunctions -> m ()
loadLibrary (Inline script) = void $ eval_ ("Lib = {};\n" <> script)
loadLibrary External{..} = do
  let libPath = toS hash <.> "js"
  lib <- liftIO $ doesFileExist libPath >>= \case
    True -> withUtf8 $ BS.readFile libPath
    False ->
      -- get the contents (as a lazy ByteString)
      case parseURI $ toS url of
        Nothing -> error $ "Not a valid URL " ++ color blue ("'" ++ toS url ++ "'") ++ "."
        Just u  -> do
          contents <- simpleHTTP (mkRequest GET u) >>= getResponseBody
          let contents_hash = toS $ BS.toLazyByteString $ BS.byteStringHex $ hashlazy contents

          unless (contents_hash == hash) $ throwM $ HashMismatch {
              url = url
            , expected_hash = toS hash
            , found_hash = toS contents_hash
            }
          BSL.writeFile libPath contents
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
convertRow :: (MonadReader JSContextPtr m, MonadMask m, MonadIO m) =>
     Int -- ^ Row index/counter
  -> Value -- ^ Row data as JSON
  -> header -- ^ Row header
  -> (JSValue -> header -> m Value) -- ^ Coverter function taking the row and header returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> DataOutput -- ^ Either a Database connection or a file handle, 
                -- depending on where we output data
  -> ErrorOutput
  -> TerminateOnError
  -> m (Int, HM.HashMap Value (S.HashSet Value)) -- ^ returns a map from JSON 'Value's to a set of 'Value's, 
                                                    -- by calling 'createAllPathsWithValues' on the converter function output
convertRow i rowJSON header rowFun validator outputHandle onError terminateOnError = 
  try ((withJSValue rowJSON $ \row -> rowFun row header) `catch` \(e::SomeJSRuntimeException) -> throwM $ JSRuntimeError (show e)) >>= \case
    -- Below, we catch in two stages, to get better errors. Notice the second catch in 
    -- the Right branch has the additional parameter res, which is added to the error
    -- output for easier debugging.
    Left (e::SomeRuntimeException) -> handleError terminateOnError onError i e rowJSON "" >> return (0, HM.empty)
    Right (Array resMultiple) -> 
      -- if rowFun returns an array of records, we process each one and merge the resulting
      -- jsonAttrVals from each run.
      ifoldM (\j (countAcc, jsonAttrValsAcc) res -> 
        do
          (c, jsonAttrVals) <- process j res
          return (countAcc+c, HM.unionWith S.union jsonAttrVals jsonAttrValsAcc)
        ) (0, HM.empty) resMultiple 
    Right res -> process (0::Int) res
  where
    process j res = do {
      validate validator res ;
      subjectID <- getSubjectID res ;
      liftIO $ case outputHandle of 
        DBOutput (con :: con_t) sourceID fileID -> do
          -- insert record into the JSONB table
          case dbType @con_t of 
            Postgres -> Postgres.insertJSONBOverwriteOnConflict sourceID fileID subjectID res con
            _ -> pure ()
          -- flatten record into EAV and insert into the EAV table
          (_,eav) <- flattenToEAV res
          forM_ eav $ \(uuid,attr,val) -> insertEAV uuid sourceID fileID subjectID attr val con
          return (1, createAllPathsWithValues res)
        JSONFileOutput outputFileHandle -> do
          when (i > 0 || j > 0) $ BSL.hPutStr outputFileHandle " , "
          BSL.hPutStr outputFileHandle $ encodePretty res
          return (1, HM.empty) ;
        ConsoleOutput -> do
          putStrLn $ toS $ encodePretty res
          return (1, HM.empty) ;
        SQLFileOutput sourceID outputFileHandle -> do
          (_,eav) <- flattenToEAV res
          forM_ eav $ \(uuid,attr,val) -> BSL.hPutStr outputFileHandle $ toS $ (fromQuery $ MySQL.insertEAVPrepareQuery uuid sourceID (FileID 0) subjectID attr val) <> ";\n"
          return (1, HM.empty) ;
    } `catch` \(e::SomeRuntimeException) -> handleError terminateOnError onError i e rowJSON res >> return (0, HM.empty)

{-|
Helper function which loops over the rows of the given input, 
collecting and merging all the resulting maps (generated inside 'convertRow' via 'createAllPathsWithValues') 
-}
processRow :: (FoldableWithIndex f, MonadThrow m) => 
       f row -- ^ Any list like data structure containing rows we can loop over with an index
    -> (Int -> row -> m (Int, HM.HashMap Value (S.HashSet Value))) -- ^ Function that consumes the row together with it's index
    -> m (Int, HM.HashMap Value (S.HashSet Value))
processRow input m = ifoldM (\i (countAcc, jsonAttrValsAcc) l -> do
    (count, jsonAttrVals) <- m i l
    return (count+countAcc, HM.unionWith S.union jsonAttrVals jsonAttrValsAcc)
  ) (0, HM.empty) input


processTxtFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOutput
  -> TerminateOnError 
  -> Int
  -> m (Int, HM.HashMap Value (S.HashSet Value))
processTxtFile rowFun validator fName outputHandle onError terminateOnError startFromLine = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  withJSValue ([("h", "data")] :: [(Text.Text, Text.Text)]) $ \header ->
    processRow (Text.lines txt) $ \i l ->
      if i < startFromLine then return (0, HM.empty)
      else let rowJSON = Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)] in 
        convertRow i rowJSON header rowFun validator outputHandle onError terminateOnError
  


processXlsxFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOutput
  -> TerminateOnError 
  -> Maybe SheetName
  -> m (Int, HM.HashMap Value (S.HashSet Value))
processXlsxFile rowFun validator fName outputHandle onError terminateOnError sheetName = do
  headerRowsMaybe <- readXlsxFile fName (fmap unSheetName sheetName)
  case headerRowsMaybe of
    Just (h, rows) ->
      withJSValue (toJSON h) $ \header ->
        processRow rows $ \i rowJSON -> 
          convertRow i rowJSON header rowFun validator outputHandle onError terminateOnError
    Nothing -> case sheetName of
      Just (SheetName s) -> throwM $ SheetNotFound s
      Nothing -> throwM $ RuntimeError $ "No sheets found in file."



processCsvFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOutput
  -> TerminateOnError 
  -> m (Int, HM.HashMap Value (S.HashSet Value))
processCsvFile rowFun validator fName outputHandle onError terminateOnError = do
  (h, rows) <- readCsvFile fName
  withJSValue h $ \header ->
    processRow rows $ \i r ->
      let rowJSON = Object $ HM.insert "i" (toJSON i) r in 
        convertRow i rowJSON header rowFun validator outputHandle onError terminateOnError


processJsonFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> DataOutput
  -> ErrorOutput
  -> TerminateOnError 
  -> m (Int, HM.HashMap Value (S.HashSet Value))
processJsonFile rowFun validator fName outputHandle onError terminateOnError = do 
  f <- liftIO $ BSL.readFile fName;
  let 
    (rows, hs) = case decode f of
      Just (Array js) -> (js, S.toList $ V.foldl' (\acc o -> (S.fromList $ getHeader o) `S.union` acc) S.empty js)
      Just o -> (V.singleton o, getHeader o)
      Nothing -> (V.empty, [])
  
  withJSValue (Object $ HM.fromList $ map (\h -> ("h", toJSON h)) hs) $ \header ->
    processRow rows $ \i r -> do
      let rowJSON = insertI i r in convertRow i rowJSON header rowFun validator outputHandle onError terminateOnError
   
  where
    getHeader (Object o) = HM.keys o
    getHeader _ = []

    insertI i (Object o) = Object $ HM.insert "i" (toJSON i) o
    insertI i o = Object $ HM.fromList [ ("i",toJSON i), ("data", o) ]





processFile :: (MonadMask m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)  -- ^ Function taking the row and header `JSValue`s, returning a JSON 'Value'
  -> (Value -> [ValidatorFailure]) -- ^ Validator function, used to check that the output of the converter function
                                   -- conforms to the JSON schema, specified in 'jsonSchema'
  -> FilePath -- ^ Path of the input file
  -> FileType  -- ^ Number of rows to skip/row number to start from only used when 'FileType' is 'TXTFile'. Default is 0.
                                           -- Optional sheet name only used when 'FileType' is 'XLSXFile'.
  -> DataOutputOpt
  -> ErrorOpt -- ^ Describes the error behaviour when parsing a row of the input. 
  -> TerminateOnError 
  -> WriteCountToDB -- ^ Write record count to DB. Assumes that the 
  -> m ()
processFile rowFun validator fName fType outOpt onError terminateOnError writeCount = 
  case outOpt of
    -- if we have db conn info, we will be writing into the db instead of a file
    DBOutputOpt (SomeDBType (db_type :: DBType ty), user, pass, host, port, db) sourceID ->
      bracket
        -- open a connection to the db
        (liftIO $ do
          dbHandle <- DB.connect user pass host port db
          logHandle <- if onError == LogToFile 
            then writeFile (fName <.> "log") "" >> Just <$> openFile (fName <.> "log") AppendMode 
            else return Nothing
          return (dbHandle, logHandle)
        ) 
        -- this action runs after completing the main body function
        (\(dbHandle, logFileHandle) -> do
          liftIO $ DB.disconnect dbHandle
          liftIO $ mapM hClose logFileHandle
        ) $
        -- main body function
        \(con :: ty, logFileHandle) -> do 
          fileID <- do
            liftIO $ getFileID sourceID (takeFileName fName) con >>= \case
              Just fileID -> do
                -- clear any errors from a possible previous run, if we are logging to db
                liftIO $ clearErrors sourceID fileID con
                return fileID
              Nothing -> throwM $ FileIDNotFound fName
          (counts, jsonAttrVals) <- process DBOutput{..} (ErrorOutput $ if onError == LogToConsole then ConsoleOutput else fromMaybe DBOutput{..} (JSONFileOutput <$> logFileHandle))
          liftIO $ case db_type of 
            Postgres -> do
              -- if writing to postgres, we run additional cleanup after inserting records into eavs_jsonb
              forM_ (HM.toList jsonAttrVals) $ \(attr,vs) ->
                Postgres.insertJSONBAttributesValuesMergeOnConflict sourceID attr (toJSON vs) con
              _ <- Postgres.cleanupJSONBAttributesValues con
              when (unWriteCountToDB writeCount) $ Postgres.updateRecordCount sourceID counts con
            MySQL -> when (unWriteCountToDB writeCount) $ MySQL.updateRecordCount sourceID counts con
    JSONFileOutputOpt -> 
      bracket
        -- open an output file and write an opening bracket '['
        (liftIO $ do
          outHandle <- writeFile (fName <.> "out.json") "[\n" >> openFile (fName <.> "out.json") AppendMode
          logHandle <- if onError == LogToFile 
            then writeFile (fName <.> "log") "" >> Just <$> openFile (fName <.> "log") AppendMode 
            else return Nothing
          return (outHandle, logHandle)
        )
        -- after the main body, write closing bracket ']' and close the file
        (\(outputFileHandle, logFileHandle) -> do
          liftIO $ BSL.hPutStr outputFileHandle "\n]" >> hClose outputFileHandle
          liftIO $ mapM hClose logFileHandle
        ) $
        -- main body
        \(outputFileHandle, logFileHandle) ->
          void $ process (JSONFileOutput outputFileHandle) (ErrorOutput $ fromMaybe ConsoleOutput $ JSONFileOutput <$> logFileHandle)
    SQLFileOutputOpt sourceID -> 
      bracket
        -- open an output file 
        (liftIO $ do
          outHandle <- writeFile (fName <.> "out.sql") "" >> openFile (fName <.> "out.sql") AppendMode
          logHandle <- if onError == LogToFile 
            then writeFile (fName <.> "log") "" >> Just <$> openFile (fName <.> "log") AppendMode 
            else return Nothing
          return (outHandle, logHandle)
        )
        -- after the main body, close the file
        (\(outputFileHandle, logFileHandle) -> do
          liftIO $ hClose outputFileHandle
          liftIO $ mapM hClose logFileHandle
        ) $
        -- main body
        \(outputFileHandle, logFileHandle) ->
          void $ process (SQLFileOutput sourceID outputFileHandle) (ErrorOutput $ fromMaybe ConsoleOutput $ JSONFileOutput <$> logFileHandle)
  where
    process outputHandle errorHandling = case fType of
      TXTFile startFromLine -> processTxtFile  rowFun validator fName outputHandle errorHandling terminateOnError startFromLine 
      XLSXFile sheetName    -> processXlsxFile rowFun validator fName outputHandle errorHandling terminateOnError sheetName
      CSVFile               -> processCsvFile  rowFun validator fName outputHandle errorHandling terminateOnError
      JSONFile              -> processJsonFile rowFun validator fName outputHandle errorHandling terminateOnError
