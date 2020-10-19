{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP, TypeOperators, DataKinds, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Main where

import           Options.Generic
import           System.IO                     (openFile, IOMode(..))
import           Main.Utf8                     (withUtf8)
import           Data.Aeson                    (decode)
import           Data.Maybe                    (fromMaybe)
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

import           Runtime
import           Quickjs
import           DB                            (ConnInfo, DBType(..), SomeDBType(..), User(..), Password(..), Host(..), Port(..), Database(..), SourceID(..), mkConnInfo)

{-|
Output options specified by the '-o' flag via the CLI (options are '-o db|sql|json')
-}
data Options w = Options
  { input     :: w ::: FilePath         <?> "Input file"
  , settings  :: w ::: FilePath         <?> "Settings file"
  , output    :: w ::: Maybe String     <?> "Output type, one of db|sql|json, defaults to json"
  , env       :: w ::: Maybe FilePath   <?> "Path to .env file used for the DB connection (must include db_type, host, dbname, user, password; the port parameter is optional)"
  , sourceId  :: w ::: Maybe Int        <?> "Souce ID used for DB insert"
  , dbConfig  :: w ::: Maybe String     <?> "DB Connection URI string in the format db_type://user:password@host:port/dbname"
  }
  deriving (Generic)

nameMod :: String -> Maybe Char
nameMod "source-id" = Nothing
nameMod "db-config" = Nothing
nameMod s = firstLetter s

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers{ 
    shortNameModifier = nameMod
  }
    
deriving instance Show (Options Unwrapped)


mkDataOutputOpt :: Maybe ConnInfo -> Maybe SourceID -> Maybe String -> DataOutputOpt
mkDataOutputOpt (Just connInfo) (Just sourceID) (Just "db")   = DBOutputOpt connInfo sourceID
mkDataOutputOpt _               (Just sourceID) (Just "sql")  = SQLFileOutputOpt sourceID
mkDataOutputOpt _               Nothing         (Just "sql")  = error "--source-id argument is required when writing to an SQL file."
mkDataOutputOpt _               _               (Just "json") = JSONFileOutputOpt
mkDataOutputOpt _               _               (Just u)      = error $ "Invalid data output option: " ++ toS u
mkDataOutputOpt _               _               Nothing       = JSONFileOutputOpt


color :: String -> String
#if defined(mingw32_HOST_OS)
color s = s
#else
color s = "\ESC[032m" ++ s ++ "\ESC[0m"
#endif

main :: IO ()
main = withUtf8 $ do
  putStrLn $ color $ 
    "                                             _   \n                                            | |  \n  _____   _____ ___ ___  _ ____   _____ _ __| |_ \n / __\\ \\ / ____/ __/ _ \\| '_ \\ \\ / / _ \\ '__| __|\n| (__ \\ V /   | (_| (_) | | | \\ V /  __/ |  | |_ \n \\___| \\_/     \\___\\___/|_| |_|\\_/ \\___|_|   \\__| v" ++
    showVersion version ++
    "\n                                                    \n                                                    "
    
  Options{..} <- unwrapRecord "CV convert CLI"
  let envPath = fromMaybe "./.env" env
  envExists <- doesFileExist envPath
  if envExists then do
    loadEnvFromAbsolute envPath 
    liftIO $ putStrLn ("Loaded env..." :: String)
  else pure ()

  dbConnInfo <- 
    if output == (Just "db") then 
      case dbConfig of
        Just s -> return $ mkConnInfo s
        Nothing ->
          if envExists then do
            when (sourceId == Nothing) $ error "--source-id argument is required when inserting into DB"
            [db_type, host, db, user, pass] <- mapM (\e -> do
              v <- lookupEnv e
              case v of 
                Just var -> return var
                Nothing -> error $ "Variable " ++ e ++ " is not defined.") ["db", "host", "dbname", "user", "password"]
            port <- lookupEnv "port"
            case db_type of
              "postgres" -> return $ Just (SomeDBType Postgres, User user, Password pass, Host host, Port port, Database db)
              "postgresql" -> return $ Just (SomeDBType Postgres, User user, Password pass, Host host, Port port, Database db)
              "mysql" -> return $ Just (SomeDBType MySQL, User user, Password pass, Host host, Port port, Database db)
              other -> error $ "DB type " ++ other ++ " is not supported."
          else error "No DB connection parameters were provided. Please specify these using the --db-config flag or inside a .env file."
    else return Nothing
  
  let outOpt = mkDataOutputOpt dbConnInfo (SourceID <$> sourceId) output


  fileSettings <- openFile settings ReadMode
  txtSettings <- BS.hGetContents fileSettings
  case (decode txtSettings :: Maybe Settings) of
    Just Settings{..} -> do
      case compileSchema jsonSchema of
        Left e -> do
          putStrLn "Invalid schema"
          putStrLn $ show e
        Right validator ->
          quickjs $ do
            mapM_ loadLibrary libraryFunctions
            _ <- eval_ $ "rowFun = (row, header) => { " <> toS processFunction <> " }"
            
            processFile
              rowFun 
              validator 
              input 
              (fromSpecifiedFileType (tail $ takeExtension input) openAs)
              outOpt
              (fromMaybe LogToConsole onError) 
              
    Nothing -> putStrLn "Invalid Settings file"

  where
    rowFun row header = call "rowFun" [row, header]