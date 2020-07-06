{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import           Options.Generic
import           Text.Regex.PCRE.Heavy
import           System.IO                    (openFile, IOMode(..), Handle, hPutStrLn, hClose)
import           Data.Aeson                   (Value(..), FromJSON(..), decode, toJSON, genericParseJSON, defaultOptions, constructorTagModifier, camelTo2)
import           Data.Aeson.Encode.Pretty     (encodePretty)
import           Data.Maybe                   (fromMaybe)
import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad                (when, forM_)
import qualified Data.HashMap.Strict          as HM
import           Control.Monad.Except         (MonadError, catchError, throwError)
import           Control.Monad.Reader         (MonadReader)
import           Control.Lens.Combinators     (FoldableWithIndex, ifoldlM)
import           Data.Foldable                (foldlM)
import           Data.List                    (intercalate)
import           System.FilePath.Posix        (takeFileName, takeExtension, (-<.>))
import           Data.Char                    (toLower)
import           Data.String.Conv             (toS)
import           System.Directory             (doesFileExist)
import           LoadEnv                      (loadEnvFromAbsolute)
import           System.Environment           (lookupEnv)
import           Data.Scientific              (toBoundedInteger)
import qualified Data.HashSet                  as S
import qualified Data.Vector                   as V
import           Database.HDBC.PostgreSQL.Pure(Config(..), Address(..), Connection, connect)
import           Database.HDBC.Types          (IConnection(commit, disconnect))
import           Data.Default.Class           (def)


import           Quickjs
import           Schema
import           Parse.Xlsx
import           Parse.Csv
import           DB
import           JSON.Utils




data Options w = Options
  { input     :: w ::: FilePath       <?> "Input file"
  , settings  :: w ::: FilePath       <?> "Settings file"
  , db        :: w ::: Bool           <?> "Output data to DB"
  -- , output   :: w ::: Maybe FilePath <?> "Output file"
  -- , lib       :: w ::: Maybe FilePath <?> "Path to JS lib"
  , env       :: w ::: Maybe FilePath <?> "Path to .env file used for DB connection (must include host, port, dbname, user, password) and other settings such as the path to the js lib."
  , source_id :: w ::: Maybe Int      <?> "Souce ID used for DB insert"
  }
  deriving (Generic)


modifiers :: Modifiers
modifiers = defaultModifiers{ 
    shortNameModifier = firstLetter
  }

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (Options Unwrapped)


data FileType = TXT | JSON | CSV | XLSX deriving (Show, Eq, Generic)
instance FromJSON FileType where
  parseJSON = genericParseJSON defaultOptions{ constructorTagModifier = map toLower }


readFileType :: String -> Maybe FileType
readFileType [] = Nothing
readFileType s = decode $ toS $ "\"" ++ (map toLower $ tail s) ++ "\""

data ErrorOpts = Terminate | LogToConsole | LogToFile deriving (Show, Eq, Generic)
instance FromJSON ErrorOpts where
  parseJSON = genericParseJSON defaultOptions {
                constructorTagModifier = camelTo2 '_' }

data Settings = Settings { processFunction :: String
                         , jsonSchema :: Maybe Schema
                         , openAs :: Maybe FileType
                         , startFrom :: Maybe Int
                         , onError :: Maybe ErrorOpts
                         } deriving (Show, Eq, Generic)
 
instance FromJSON Settings


loadLibrary :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) => FilePath -> m ()
loadLibrary libPath = do
  liftIO $ print $ "lib path: " ++ libPath

  libExists <- liftIO $ doesFileExist libPath
  when libExists $ do
    f <- liftIO $ readFile libPath
    let f' = gsub [re|export\s+default|] ("" :: String) f
    _ <- eval Global $ f'
    liftIO $ print $ "loaded lib"
    return ()
  return ()

main :: IO ()
main = do
  Options{..} <- unwrapRecord "CV convert CLI"
  let envPath = fromMaybe "./env" env
  envExists <- doesFileExist envPath
  if envExists then do
    loadEnvFromAbsolute envPath 
    liftIO $ print $ "loaded env"
  else pure ()

  dbConnInfo <- 
    if db then do
      when (source_id == Nothing) $ error "source_id is required when inserting into DB"
      [host, database, user, password] <- mapM (\e -> do
        v <- lookupEnv e
        case v of 
          Just var -> return var
          Nothing -> error $ "Variable " ++ e ++ " is not defined.") ["host", "dbname", "user", "password"]
      
      port <- lookupEnv "port"
      let address = AddressNotResolved host (fromMaybe "5432" port)
      return $ Just def{address = address , database = database , user = user , password = password}
    else return Nothing

  fileSettings <- openFile settings ReadMode
  txtSettings <- BS.hGetContents fileSettings
  case (decode txtSettings :: Maybe Settings) of
    Just Settings{..} -> do
      case compileSchema jsonSchema of
        Left e -> do
          putStrLn "Invalid schema"
          putStrLn $ show e
        Right validator ->
          let fileType = fromMaybe (fromMaybe TXT $ readFileType $ takeExtension input) openAs in 
          quickjsIO $ do
            lib <- liftIO $ lookupEnv "jslib"
            loadLibrary (fromMaybe "./lib.js" lib)
            _ <- eval Global $ "rowFun = (row, header) => { " ++ processFunction ++ " }"
            
            processFile dbConnInfo source_id rowFun validator input fileType (fromMaybe LogToConsole onError) (fromMaybe 0 startFrom)
    Nothing -> putStrLn "Invalid Settings file"

  where
    rowFun row header = do
      r <- call "rowFun" [row, header] 
      res <- fromJSValue_ r
      freeJSValue r
      return res

processFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) =>
     Maybe Config
  -> Maybe Int
  -> (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> FileType
  -> ErrorOpts
  -> Int
  -> m ()
processFile dbConnInfo source_id rowFun validator fName fType onError startFromLine = do
  logFile <- liftIO $ case onError of
    LogToFile -> writeFile (fName -<.> "log") "" >> openFile (fName -<.> "log") AppendMode >>= return . Just
    _ -> return Nothing
  

  -- if we have db conn info, we will be writing into the db instead of a file
  case (dbConnInfo, source_id) of
    (Just c, Just srcID) -> do
      con <- liftIO $ connect c
      fileID <- liftIO $ getFileID srcID (takeFileName fName) con
      
      _ <- case fileID of
        Just i -> do
          jsonAttrVals <- process (Left (con, srcID, i)) logFile
          liftIO $ forM_ (HM.toList jsonAttrVals) $ \(attr,vs) ->
            insertJSONBAttributesValuesMergeOnConflict srcID attr (toJSON vs) con
          liftIO $ cleanupJSONBAttributesValues con
        Nothing -> error $ "File ID for '" ++ (takeFileName fName) ++ "' could not be found."
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
      XLSX -> processXlsxFile rowFun validator fName logFile onError outputHandle
      CSV  -> processCsvFile  rowFun validator fName logFile onError outputHandle
      JSON -> processJsonFile rowFun validator fName logFile onError outputHandle


getSubjectID :: MonadError String m => Value -> m String
getSubjectID (Object m) = case HM.lookup "subject_id" m of
  Just x -> return (show x)
  Nothing -> throwError "subject_id is required"
getSubjectID o = throwError $ "Expected " ++ show o ++ " to be an object."


processRow :: (FoldableWithIndex i f, MonadError String m, MonadIO m, MonadReader JSContextPtr m) => 
  f a -> (i -> a -> m (Maybe (HM.HashMap Value (S.HashSet Value)))) -> m (HM.HashMap Value (S.HashSet Value))
processRow input m = ifoldlM (\i jsonAttrValsAcc l -> do
    jsonAttrValsMaybe <- m i l
    case jsonAttrValsMaybe of 
      Just jsonAttrVals -> return $ HM.unionWith (S.union) jsonAttrVals jsonAttrValsAcc
      Nothing -> return jsonAttrValsAcc
  ) HM.empty input


processTxtFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Int
  -> Either (Connection, Int, Int) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processTxtFile rowFun validator fName logFile onError startFromLine outputHandle = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  header <- toJSValue ([("h", "data")] :: [(Text.Text, Text.Text)])

  res <- processRow (Text.lines txt) $ \i l ->
    if (i < startFromLine) then 
      return Nothing
    else do
      row <- toJSValue (Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)])
      row_res <- parse i row header rowFun validator outputHandle onError logFile
      freeJSValue row
      return row_res
  
  freeJSValue header
  return res


processXlsxFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, Int, Int) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processXlsxFile rowFun validator fName logFile onError outputHandle = do
  headerRowsList <- readXlsxFile fName
  foldlM (\jsonAttrValsAcc (h,rows) -> do {
    header <- toJSValue (toJSON h) ;
    jsonAttrValsAcc' <- processRow rows $ \i r -> do {
      row <- toJSValue r ;
      row_res <- parse i row header rowFun validator outputHandle onError logFile ;
      freeJSValue row ;
      return row_res
    };
    freeJSValue header ;
    return $ HM.unionWith (S.union) jsonAttrValsAcc jsonAttrValsAcc'
  }) HM.empty headerRowsList


processCsvFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, Int, Int) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processCsvFile rowFun validator fName logFile onError outputHandle = do
  (h, rows) <- readCsvFile fName
  header <- toJSValue h

  res <- processRow rows $ \i r -> do
    row <- toJSValue $ Object $ HM.insert "i" (toJSON i) r
    row_res <- parse i row header rowFun validator outputHandle onError logFile
    freeJSValue row
    return row_res
  freeJSValue header
  return res


processJsonFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m) =>
     (JSValue -> JSValue -> m Value)
  -> (Value -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Either (Connection, Int, Int) Handle
  -> m (HM.HashMap Value (S.HashSet Value))
processJsonFile rowFun validator fName logFile onError outputHandle = do 
  f <- liftIO $ BS.readFile fName;
  let 
    (rows, hs) = case decode f of
      Just (Array js) -> (V.toList js, S.toList $ V.foldl (\acc o -> (S.fromList $ getHeader o) `S.union` acc) S.empty js)
      Just o -> ([o], getHeader o)
      Nothing -> ([], [])
  
  header <- toJSValue $ Object $ HM.fromList $ map (\h -> ("h", toJSON h)) hs

  res <- processRow rows $ \i r -> do
    row <- toJSValue $ insertI i r
    row_res <- parse i row header rowFun validator outputHandle onError logFile
    freeJSValue row
    return row_res
  freeJSValue header
  return res
  

  where
    getHeader (Object o) = HM.keys o
    getHeader _ = []

    insertI i (Object o) = Object $ HM.insert "i" (toJSON i) o
    insertI i o = Object $ HM.fromList [ ("i",toJSON i), ("data", o) ]

parse :: (MonadError String m, MonadIO m, Ord i, Num i, Integral i, Show i) =>
     i
  -> row
  -> header
  -> (row -> header -> m Value)
  -> (Value -> [ValidatorFailure])
  -> Either (Connection, Int, Int) Handle
  -> ErrorOpts
  -> Maybe Handle
  -> m (Maybe (HM.HashMap Value (S.HashSet Value)))
parse i row header rowFun validator outputHandle onError logFile = do {  
  res <- rowFun row header;
  validate validator res ;
  subjectID <- getSubjectID res ;
  liftIO $ case outputHandle of 
    Left (con, srcID, fileID) -> do
      -- insert record into the JSONB table
      insertJSONBOverrideOnConflict srcID fileID subjectID res con
      -- flatten record into EAV and insert into the EAV table
      (_,eav) <- flattenToEAV res
      forM_ eav $ \(uuid,attr,val) -> insertEAV uuid (show srcID) fileID subjectID attr val con

      return $ Just $ createAllPathsWithValues res
    Right outputFile -> do
      when (i > 0) $ BS.hPutStr outputFile " , "
      BS.hPutStr outputFile $ encodePretty res
      return Nothing ;
} `catchError` (\e -> handleError onError logFile i e >> return Nothing)


validate :: MonadError String m => (Value -> [ValidatorFailure]) -> Value -> m ()
validate validator res = case validator res of
  [] -> return ()
  errors -> throwError $ intercalate "\n\n" $ map pValidatorFailure errors

handleError :: (MonadError String m, Show i, MonadIO m) =>
  ErrorOpts -> Maybe Handle -> i -> String -> m ()
handleError Terminate _ lineNo err = 
  throwError $ "Terminating with error on line " ++ show lineNo ++ ":\n" ++ err
handleError LogToConsole _ lineNo err = 
  liftIO $ putStrLn $ "Error on line " ++ show lineNo ++ ": " ++ err
handleError LogToFile (Just logFile) lineNo err = 
  liftIO $ hPutStrLn logFile $ "Error on line " ++ show lineNo ++ ": " ++ err
handleError LogToFile Nothing _ _ = undefined 
