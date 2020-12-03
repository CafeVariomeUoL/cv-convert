{-# LANGUAGE ScopedTypeVariables, DerivingStrategies, TypeOperators, DataKinds, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Generic
import           System.IO                     (openFile, IOMode(..))
import           System.IO.Error               (ioeGetFileName, ioeGetErrorType)
import           GHC.IO.Exception              (ExitCode, IOErrorType(..))
import           Main.Utf8                     (withUtf8)
import qualified Control.Exception             as E
import           Data.Aeson                    (decode)
import           Data.Maybe                    (isNothing, fromMaybe)
import qualified Data.ByteString.Lazy          as BS
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad                 (when)
import           System.FilePath.Posix         (takeExtension)
import           System.Directory              (doesFileExist)
import           LoadEnv                       (loadEnvFromAbsolute)
import           System.Environment            (lookupEnv)
import           Data.String.Conv              (toS)
import           Paths_cv_convert              (version)
import           Data.Version                  (showVersion)
import           Data.IORef                    (IORef, newIORef, writeIORef, readIORef)

import           Runtime
import           Runtime.Error
import           Runtime.Utils
import           Quickjs
import           DB                            (ConnInfo, DBType(..), SomeDBType(..), SomeDBException(..), User(..), Password(..), Host(..), Port(..), Database(..), SourceID(..), mkConnInfo)

{-|
Output options specified by the '-o' flag via the CLI (options are '-o db|sql|json')
-}
data Options w = Options
  { input            :: w ::: FilePath         <?> "Input file"
  , settings         :: w ::: FilePath         <?> "Settings file"
  , output           :: w ::: Maybe String     <?> "Output type, one of db|sql|json, defaults to json"
  , env              :: w ::: Maybe FilePath   <?> "Path to .env file used for the DB connection (must include db_type, host, dbname, user, password; the port parameter is optional)"
  , sourceId         :: w ::: Maybe Int        <?> "Souce ID used for DB insert"
  , dbConfig         :: w ::: Maybe String     <?> "DB Connection URI string in the format db_type://user:password@host:port/dbname"
  , log              :: w ::: Maybe String     <?> "Logging ebehaviour, one of console|file|db, defaults to console"
  , terminateOnError :: w ::: Bool             <?> "Terminate when encountering an error. Defaults to false"
  , writeRecordCount :: w ::: Bool             <?> "Write the number of records added to the database into the 'sources' table"
  , verbose          :: w ::: Bool             <?> "Show verbose error messages."
  }
  deriving (Generic)

nameMod :: String -> Maybe Char
nameMod s
  | s == "sourceId" = Nothing
  | s == "dbConfig" = Nothing
  | s == "log" = Nothing
  | s == "terminateOnError" = Nothing
  | s == "writeRecordCount" = Nothing
  | s == "env" = Nothing
  | otherwise = firstLetter s

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers{ 
    shortNameModifier = nameMod
  }
    
deriving instance Show (Options Unwrapped)


mkDataOutputOpt :: Maybe ConnInfo -> Maybe SourceID -> Maybe String -> DataOutputOpt
mkDataOutputOpt (Just connInfo) (Just sourceID) (Just "db")   = DBOutputOpt connInfo sourceID
mkDataOutputOpt _               Nothing         (Just "db")   = error $ (color blue $ "--source-id") ++ " argument is required when inserting into DB."
mkDataOutputOpt Nothing         _               (Just "db")   = error $ "No DB connection parameters were provided. Please specify these using the " ++ (color blue "--db-config") ++ " flag or inside a .env file."
mkDataOutputOpt _               (Just sourceID) (Just "sql")  = SQLFileOutputOpt sourceID
mkDataOutputOpt _               Nothing         (Just "sql")  = error $ (color blue $ "--source-id") ++ " argument is required when writing to an SQL file."
mkDataOutputOpt _               _               (Just "json") = JSONFileOutputOpt
mkDataOutputOpt _               _               (Just s)      = error $ "Invalid argument " ++ (color blue $ "--output " ++ s) ++ ". Must be one of " ++ (color blue "db|sql|json") ++ "." 
mkDataOutputOpt _               _               Nothing       = JSONFileOutputOpt

mkErrorOpt :: Maybe String -> ErrorOpt
mkErrorOpt (Just "db")        = LogToDB
mkErrorOpt (Just "file")      = LogToFile
mkErrorOpt (Just "console")   = LogToConsole
mkErrorOpt (Just s)           = error $ "Invalid argument " ++ (color blue $ "--log " ++ s) ++ ". Must be one of " ++ (color blue "terminate|console|file|db") ++ "." 
mkErrorOpt Nothing            = LogToFile


cli :: IORef Bool -> IO ()
cli v = withUtf8 $ do
  putStrLn $ color green $ 
    "                                             _   \n                                            | |  \n  _____   _____ ___ ___  _ ____   _____ _ __| |_ \n / __\\ \\ / ____/ __/ _ \\| '_ \\ \\ / / _ \\ '__| __|\n| (__ \\ V /   | (_| (_) | | | \\ V /  __/ |  | |_ \n \\___| \\_/     \\___\\___/|_| |_|\\_/ \\___|_|   \\__| v" ++
    showVersion version ++
    "\n                                                    \n                                                    "
    
  Options{..} <- unwrapRecord "CV convert CLI"

  when verbose $ writeIORef v True

  let envPath = fromMaybe "./.env" env
  envExists <- doesFileExist envPath
  if envExists then do
    loadEnvFromAbsolute envPath 
    liftIO $ putStrLn $ color blue "ℹ️  Loaded env...\n"
  else pure ()

  dbConnInfo <- 
    if output == (Just "db") then 
      case dbConfig of
        Just s -> do
          let connInfo = mkConnInfo s
          when (isNothing connInfo) $ error $ "The connection parameter " ++ (color blue s) ++ " is invalid."
          return $ connInfo
        Nothing ->
          if envExists then do
            [db_type, host, db, user, pass] <- mapM (\e -> do
              var <- lookupEnv e
              case var of 
                Just var' -> return var'
                Nothing -> error $ "Variable " ++ (color blue e) ++ " is not defined.") ["db", "host", "dbname", "user", "password"]
            port <- lookupEnv "port"
            case db_type of
              "postgres"   -> return $ Just (SomeDBType Postgres, User user, Password pass, Host host, Port port, Database db)
              "postgresql" -> return $ Just (SomeDBType Postgres, User user, Password pass, Host host, Port port, Database db)
              "mysql"      -> return $ Just (SomeDBType MySQL, User user, Password pass, Host host, Port port, Database db)
              other        -> error $ "DB type " ++ other ++ " is not supported."
          else error $ "No DB connection parameters were provided. Please specify these using the " ++ (color blue "--db-config") ++ " flag or inside a .env file."
    else return Nothing
  
  let outOpt = mkDataOutputOpt dbConnInfo (SourceID <$> sourceId) output
      errorOpt = mkErrorOpt log

  when (errorOpt == LogToDB && isNothing dbConnInfo) $ 
    error $ "Cannot use " ++ (color blue "--log db") ++ ", because no DB connection parameters were provided. Please specify these using the " ++ (color blue "--db-config") ++ " flag or via a .env file."

  fileSettings <- openFile settings ReadMode
  txtSettings <- BS.hGetContents fileSettings
  case (decode txtSettings :: Maybe Settings) of
    Just Settings{..} -> do
      case compileSchema jsonSchema of
        Left e -> do
          error $ "Invalid schema:\n" ++ show e
        Right validator -> do
          quickjs $ do
            mapM_ loadLibrary libraryFunctions
            _ <- eval_ $ "rowFun = (row, header) => { " <> toS processFunction <> " }"
            
            processFile
              rowFun 
              validator 
              input 
              (fromSpecifiedFileType (tail $ takeExtension input) openAs)
              outOpt
              errorOpt
              (TerminateOnError terminateOnError)
              (WriteCountToDB writeRecordCount)
          putStrLn $ color green "✅ Done..."
              
    Nothing -> error "Invalid Settings file."

  where
    rowFun row header = call "rowFun" [row, header]


main :: IO ()
main = do
  verbose <- newIORef False
  cli verbose
    `E.catch` (\(E.ErrorCall e) -> putStrLn $ color red "🚨 CLI Error: " ++ e)
    `E.catch` (\((SomeDBException simple e)) -> do
      v <- readIORef verbose
      putStrLn $ color red "🚨 DB Error: " ++ simple ++ (if v then "\n\n" ++ show e else "\n\nUse the " ++ (color blue "-v") ++ " flag to get a more detailed error."))
    `E.catch` (\(e :: E.IOException) -> do
      v <- readIORef verbose
      case (ioeGetFileName e, ioeGetErrorType e) of
        (Just file, NoSuchThing) -> putStrLn $ color red "🚨 IO Error: " ++ "File " ++ (color blue $ "'" ++ file ++ "'") ++ " does not exist."
        (Just file, ResourceBusy) -> putStrLn $ color red "🚨 IO Error: " ++ "Cannot open file " ++ (color blue $ "'" ++ file ++ "'") ++ ". Resource is busy." ++ (if v then "\n\n" ++ show e else "\n\nUse the " ++ (color blue "-v") ++ " flag to get a more detailed error.")
        (Just file, IllegalOperation) -> putStrLn $ color red "🚨 IO Error: " ++ "Illegal operation on file " ++ (color blue $ "'" ++ file ++ "'") ++ "." ++ (if v then "\n\n" ++ show e else "\n\nUse the " ++ (color blue "-v") ++ " flag to get a more detailed error.")
        (Just file, PermissionDenied) -> putStrLn $ color red "🚨 IO Error: " ++ "Cannot open file " ++ (color blue $ "'" ++ file ++ "'") ++ ". Permission denied." ++ (if v then "\n\n" ++ show e else "\n\nUse the " ++ (color blue "-v") ++ " flag to get a more detailed error.")
        _ -> putStrLn $ color red "🚨 IO Error: " ++ show e)
    `E.catch` (\(RowError lineNo err inp out) -> putStrLn $ (color red $ "🚨 Runtime Error: [error on line " ++ show lineNo ++ "] ") ++ showColor err ++ 
      "\nInput:  " ++ (colorJSON inp) ++ "\nOutput: " ++ (colorJSON out) ++ "\n")
    `E.catch` (\(e :: SomeRuntimeException) -> putStrLn $ color red "🚨 Runtime Error: " ++ showColor e)
    `E.catch` (\(_ :: ExitCode) -> return ())
    `E.catch` (\(e :: E.SomeException) -> putStrLn $ color red "🚨 Unknown Error: " ++ show e)