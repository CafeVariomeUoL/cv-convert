{-# LANGUAGE CPP, TypeOperators, DataKinds, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

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
import           Database.HDBC.PostgreSQL.Pure as Postgres
import           Database.MySQL.Base           as MySQL
import           Data.Default.Class            (def)
import           Data.String.Conv              (toS)
import           Paths_cv_convert              (version)
import           Data.Version                  (showVersion)

import           Runtime
import           Runtime.Error
import           Quickjs
import           DB                            (SourceID(..))


instance ParseField SourceID

instance Read OutputOpt where
  readsPrec _ ('d':'b':therest) = [(DB,therest)]
  readsPrec _ ('s':'q':'l':therest) = [(SQL,therest)]
  readsPrec _ ('j':'s':'o':'n':therest) = [(JSON,therest)]
  readsPrec _ _ = []

instance ParseField OutputOpt



data Options w = Options
  { input     :: w ::: FilePath         <?> "Input file"
  , settings  :: w ::: FilePath         <?> "Settings file"
  , output    :: w ::: Maybe OutputOpt  <?> "Output type"
  , env       :: w ::: Maybe FilePath   <?> "Path to .env file used for the DB connection (must include host, port, dbname, user, password)"
  , source_id :: w ::: Maybe Int        <?> "Souce ID used for DB insert"
  }
  deriving (Generic)

nameMod :: String -> Maybe Char
nameMod "source_id" = Nothing
nameMod s = firstLetter s

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers defaultModifiers{ 
    shortNameModifier = nameMod
  }
    

deriving instance Show (Options Unwrapped)


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
  let outOpt = fromMaybe Runtime.JSON output

  dbConnInfo <- 
    if outOpt == DB then do
      when (source_id == Nothing) $ error "source_id is required when inserting into DB"
      [db_type, host, database, user, password] <- mapM (\e -> do
        v <- lookupEnv e
        case v of 
          Just var -> return var
          Nothing -> error $ "Variable " ++ e ++ " is not defined.") ["db", "host", "dbname", "user", "password"]
      
      port <- lookupEnv "port"

      case db_type of
        "postgres" -> do
          let address = Postgres.AddressNotResolved host (fromMaybe "5432" port)
          return $ Just $ Left def{address = address , database = database , user = user , password = password}
        "mysql" -> 
          return $ Just $ Right MySQL.defaultConnectInfo{
            ciHost = host
          , ciPort = fromMaybe 3306 $ fmap read port
          , ciDatabase = toS database
          , ciUser = toS user
          , ciPassword = toS password
          }
        other -> error $ "DB type " ++ other ++ " is not supported."
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
          let fileType = fromMaybe (fromMaybe (TXTFile ()) $ readFileType $ takeExtension input) openAs in 
          quickjs $ do
            mapM_ loadLibrary libraryFunctions
            _ <- eval_ $ "rowFun = (row, header) => { " <> toS processFunction <> " }"
            
            processFile 
              dbConnInfo
              (SourceID <$> source_id)
              outOpt
              rowFun 
              validator 
              input 
              (addOptsToFileType (fromMaybe 0 startFrom) () () worksheet fileType)
              (onErrorBehaviour dbConnInfo onError) 
              
    Nothing -> putStrLn "Invalid Settings file"

  where
    rowFun row header = call "rowFun" [row, header]

    -- if we specifed logging in the settings file, we use that setting
    -- if we have a DB connection and haven't explicitly specified logging, we default to logging erros to db
    -- otherwise we log to console 
    onErrorBehaviour dbConnInfo onError = case (dbConnInfo, onError) of
      (_, Just e) -> e
      (Just _, Nothing) -> LogToDb ()
      (Nothing, Nothing) -> LogToConsole
   

