{-# LANGUAGE CPP, TypeOperators, DataKinds, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import           Options.Generic
import           System.IO                    (openFile, IOMode(..))
import           Main.Utf8                    (withUtf8)
import           Data.Aeson                   (decode)
import           Data.Maybe                   (fromMaybe)
import qualified Data.ByteString.Lazy         as BS
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad                (when)
import           Control.Monad.Zip            (mzip)
import           System.FilePath.Posix        (takeExtension)
import           System.Directory             (doesFileExist)
import           LoadEnv                      (loadEnvFromAbsolute)
import           System.Environment           (lookupEnv)
import           Database.HDBC.PostgreSQL.Pure(Config(..), Address(..))
import           Data.Default.Class           (def)
import           Data.String.Conv             (toS)
import           Paths_cv_convert             (version)
import           Data.Version                 (showVersion)

import           Runtime
import           Runtime.Error
import           Quickjs


instance ParseField SourceID

data Options w = Options
  { input     :: w ::: FilePath       <?> "Input file"
  , settings  :: w ::: FilePath       <?> "Settings file"
  , db        :: w ::: Bool           <?> "Output data to DB"
  -- , output   :: w ::: Maybe FilePath <?> "Output file"
  -- , lib       :: w ::: Maybe FilePath <?> "Path to JS lib"
  , env       :: w ::: Maybe FilePath <?> "Path to .env file used for the DB connection (must include host, port, dbname, user, password)"
  , source_id :: w ::: Maybe SourceID <?> "Souce ID used for DB insert"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers defaultModifiers{ 
    shortNameModifier = firstLetter
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
    "                                                _   \n                                               | |  \n  _____   ________ ___ ___  _ ____   _____ _ __| |_ \n / __\\ \\ / /______/ __/ _ \\| '_ \\ \\ / / _ \\ '__| __|\n| (__ \\ V /      | (_| (_) | | | \\ V /  __/ |  | |_ \n \\___| \\_/        \\___\\___/|_| |_|\\_/ \\___|_|   \\__| v" ++
    showVersion version ++
    "\n                                                    \n                                                    "
    
  Options{..} <- unwrapRecord "CV convert CLI"
  let envPath = fromMaybe "./env" env
  envExists <- doesFileExist envPath
  if envExists then do
    loadEnvFromAbsolute envPath 
    liftIO $ print ("loaded env" :: String)
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
          let fileType = fromMaybe (fromMaybe (TXT ()) $ readFileType $ takeExtension input) openAs in 
          quickjs $ do
            mapM_ loadLibrary libraryFunctions
            _ <- eval_ $ "rowFun = (row, header) => { " <> toS processFunction <> " }"
            
            processFile 
              (mzip dbConnInfo source_id)
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
   

