{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}

import System.IO(openFile, IOMode(..), Handle, hPutStrLn, hClose)
import Data.Aeson(Value(..), ToJSON, FromJSON(..), decode, toJSON, genericParseJSON, defaultOptions, constructorTagModifier, camelTo2)
import Data.Aeson.Encode.Pretty(encodePretty)
import Data.Maybe(fromMaybe)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad(when)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except(MonadError, catchError, throwError)
import Control.Monad.Reader(MonadReader)
import Control.Lens.Combinators(ifor_, FoldableWithIndex, TraversableWithIndex, FunctorWithIndex)
import Data.List(intercalate)
import System.FilePath.Posix(takeExtension, (-<.>))
import Data.Char(toLower)
import Data.String.Conv(toS)
import Data.Csv.Streaming(Records, decodeByName)

import Quickjs
import Schema
import Parse.Xlsx


data FileType = TXT | JSON | CSV | XLSX deriving (Show, Eq, Generic)
instance FromJSON FileType where
  parseJSON = genericParseJSON defaultOptions {
                constructorTagModifier = map toLower }


readFileType :: String -> Maybe FileType
readFileType [] = Nothing
readFileType s = decode $ toS $ "\"" ++ (map toLower $ tail s) ++ "\""

data ErrorOpts = Terminate | LogToConsole | LogToFile deriving (Show, Eq, Generic)
instance FromJSON ErrorOpts where
  parseJSON = genericParseJSON defaultOptions {
                constructorTagModifier = camelTo2 '_' }

data Settings = Settings { processFunction :: String
                         , jsonSchema :: Schema
                         , openAs :: Maybe FileType
                         , startFrom :: Maybe Int
                         , onError :: Maybe ErrorOpts
                         } deriving (Show, Eq, Generic)
 
instance FromJSON Settings

instance FoldableWithIndex Int Records
instance TraversableWithIndex Int Records
instance FunctorWithIndex Int Records

main :: IO ()
main = do
  args <- getArgs
  if (length args > 0) then do
    let configFile = args !! 0
    let inputFile = args !! 1
    fileSettings <- openFile configFile ReadMode
    txtSettings <- BS.hGetContents fileSettings
    case (decode txtSettings :: Maybe Settings) of
      Just Settings{..} -> do
        case compileSchema jsonSchema of
          Left e -> do
            putStrLn "Invalid schema"
            putStrLn $ show e
          Right validator ->
            let fileType = fromMaybe (fromMaybe TXT $ readFileType $ takeExtension inputFile) openAs in 
            quickjsIO $ do
              _ <- eval $ "rowFun = (row) => { " ++ processFunction ++ " }"
              let rowFun row = call "rowFun" [row] >>= fromJSValue_
              processFile rowFun validator inputFile fileType (fromMaybe LogToConsole onError) (fromMaybe 0 startFrom)
      Nothing -> putStrLn "Invalid settings file"
  else putStrLn "Not enough arguments supplied"


processFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m, ToJSON res) =>
  (JSValueForeignPtr -> m res)
  -> (res -> [ValidatorFailure])
  -> FilePath
  -> FileType
  -> ErrorOpts
  -> Int
  -> m ()
processFile rowFun validator fName fType onError startFromLine = do
  logFile <- liftIO $ case onError of
    LogToFile -> writeFile (fName -<.> "log") "" >> openFile (fName -<.> "log") AppendMode >>= return . Just
    _ -> return Nothing
  
  outputFile <- liftIO $ writeFile (fName -<.> "out.json") "[\n" >> openFile (fName -<.> "out.json") AppendMode

  case fType of
    TXT -> processTxtFile rowFun validator fName logFile onError startFromLine outputFile
    XLSX -> processXlsxFile rowFun validator fName logFile onError outputFile
    CSV -> processCsvFile rowFun validator fName logFile onError outputFile

  liftIO $ do
    mapM_ hClose logFile
    BS.hPutStr outputFile "\n]"
    hClose outputFile 


processTxtFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m, ToJSON res) =>
  (JSValueForeignPtr -> m res)
  -> (res -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Int
  -> Handle
  -> m ()
processTxtFile rowFun validator fName logFile onError startFromLine outputFile = do
  file <- liftIO $ openFile fName ReadMode
  txt <- liftIO $ Text.hGetContents file
  ifor_ (Text.lines txt) $ \i l -> when (i >= startFromLine) $ do
    row <- toJSValue (Object $ HM.fromList [("i" , toJSON i), ("data" , toJSON l)])
    parse i row rowFun validator outputFile onError logFile


processXlsxFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m, ToJSON res) =>
  (JSValueForeignPtr -> m res)
  -> (res -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Handle
  -> m ()
processXlsxFile rowFun validator fName logFile onError outputFile = do
  rows <- readXlsxFile fName
  ifor_ rows $ \i r -> do
    row <- toJSValue r
    parse i row rowFun validator outputFile onError logFile


processCsvFile :: (MonadError String m, MonadIO m, MonadReader JSContextPtr m, ToJSON res) =>
  (JSValueForeignPtr -> m res)
  -> (res -> [ValidatorFailure])
  -> FilePath
  -> Maybe Handle
  -> ErrorOpts
  -> Handle
  -> m ()
processCsvFile rowFun validator fName logFile onError outputFile = do
  file <- liftIO $ decodeByName <$> BS.readFile fName
  case file of
    Left err -> throwError err
    Right (_, (rows :: Records (HM.HashMap Text.Text Text.Text))) -> ifor_ rows $ \i r -> do
      row <- toJSValue $ Object $ HM.insert "i" (toJSON i) $ HM.map toJSON r
      parse i row rowFun validator outputFile onError logFile


parse :: (MonadError String m, MonadIO m, Ord i, Num i, Show i, ToJSON res) =>
  i
  -> row
  -> (row -> m res)
  -> (res -> [ValidatorFailure])
  -> Handle
  -> ErrorOpts
  -> Maybe Handle
  -> m ()
parse i row rowFun validator outputFile onError logFile = do {  
  res <- rowFun row ;
  validate validator res ;
  liftIO $ do
    when (i > 0) $ BS.hPutStr outputFile " , "
    BS.hPutStr outputFile $ encodePretty res ;
} `catchError` handleError onError logFile i 


validate :: MonadError String m => (res -> [ValidatorFailure]) -> res -> m ()
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
