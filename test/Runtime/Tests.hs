{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, OverloadedLists #-}

module Runtime.Tests(tests) where
import           Test.Tasty                    (TestTree, testGroup, after, DependencyType(..),)
import           Test.Tasty.HUnit              (testCase, assertBool)
import           Test.HUnit                    (Assertion, (@?=))
import           Test.Tasty.Golden             (goldenVsFile)
import           Data.Aeson                    (Value(..), decode)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Catch           (try, bracket, throwM)
import           Data.Text                     (Text)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Data.String.Conv              (toS)
import           Data.Strings                  (strEndsWith, strCapitalize, strReplace)

import           Data.String.Interpolate       (i)
import           Main.Utf8                     (withUtf8)
import           Database.HDBC.PostgreSQL.Pure as Postgres
import           Database.HDBC.Types           (IConnection(commit, disconnect))
import           Database.MySQL.Base           as MySQL
import           Database.YeshQL.HDBC
import           System.FilePath.Posix         (takeBaseName, takeFileName, takeExtension, (</>), (<.>))
import           System.Directory              (getDirectoryContents, removeFile)
import           Data.Maybe                    (fromMaybe)
import           Data.List                     (sortBy)
import           Control.Exception             (SomeException)
import           Control.Monad.IO.Unlift       (MonadUnliftIO(..), UnliftIO(..), askUnliftIO)

import           Runtime
import           Runtime.Types                 (Specified(..))
import           Runtime.Error
import           Quickjs
import           Quickjs.Error
import           DB



load_lib :: Assertion
load_lib = quickjsMultithreaded $ do
  loadLibrary $ Inline "Lib.helloWorld = function() {return 2;}"
  v <- eval "Lib.helloWorld();"
  liftIO $ v @?= Number 2


[yesh1|
-- name:makeDatabaseSchemaPostgres
-- @ddl
CREATE TABLE sources (
    source_id serial primary key,
    name character varying(30) NOT NULL,
    record_count integer NOT NULL DEFAULT 0
);
CREATE TABLE eavs (
    id serial primary key,
    uid character varying(50) NOT NULL,
    source_id integer NOT NULL,
    "fileName" integer NOT NULL,
    subject_id text NOT NULL,
    type character varying(20) NOT NULL,
    attribute text NOT NULL,
    value text,
    elastic boolean DEFAULT false NOT NULL
);
CREATE TABLE eavs_jsonb (
    id serial primary key,
    source_id integer NOT NULL,
    "fileName" integer NOT NULL,
    subject_id text NOT NULL,
    data jsonb NOT NULL,
    unique(source_id,subject_id),
    CONSTRAINT eavs_jsonb_source_id_fkey FOREIGN KEY (source_id)
        REFERENCES sources (source_id) MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE NO ACTION
);
CREATE TABLE eavs_jsonb_attributes_values (
    id serial primary key,
    source_id integer NOT NULL,
    attribute jsonb NOT NULL,
    "values" jsonb NOT NULL,
    unique(source_id,attribute),
    CONSTRAINT eavs_jsonb_attributes_source_id_fkey FOREIGN KEY (source_id)
        REFERENCES sources (source_id) MATCH SIMPLE
        ON UPDATE CASCADE
        ON DELETE CASCADE
);
CREATE TABLE upload_error (
    id serial primary key,
    error_id integer NOT NULL,
    message text NOT NULL,
    error_code integer NOT NULL,
    source_id integer NOT NULL
);
CREATE TABLE uploaddatastatus (
    "FileName" character varying(40) NOT NULL,
    "uploadStart" timestamp with time zone NOT NULL,
    "uploadEnd" timestamp with time zone,
    "Status" character varying(20) NOT NULL,
    "elasticStatus" character varying(20),
    "source_id" integer NOT NULL,
    "user_id" integer NOT NULL,
    "ID" serial primary key,
    "patient" character varying(50),
    "tissue" character varying(50)
);
INSERT INTO sources(source_id, name) VALUES (1, 'test_src');
|]

makeDatabaseSchemaMySQL :: MySQL.Query
makeDatabaseSchemaMySQL = [i|
CREATE TABLE `sources` (
  `source_id` int(11) NOT NULL,
  `name` varchar(30) NOT NULL,
  `record_count` bigint(255) NOT NULL DEFAULT '0'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `eavs` (
  `id` int(15) NOT NULL,
  `uid` char(36) NOT NULL,
  `source_id` int(11) NOT NULL,
  `fileName` mediumint(8) UNSIGNED NOT NULL,
  `subject_id` varchar(20) NOT NULL,
  `type` varchar(20) NOT NULL,
  `attribute` varchar(50) NOT NULL,
  `value` varchar(200) DEFAULT NULL,
  `elastic` bit(1) NOT NULL DEFAULT b'0'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;


CREATE TABLE `uploaddatastatus` (
  `FileName` varchar(40) NOT NULL,
  `uploadStart` datetime NOT NULL,
  `uploadEnd` datetime DEFAULT NULL,
  `Status` varchar(20) NOT NULL,
  `source_id` int(11) NOT NULL,
  `user_id` mediumint(8) UNSIGNED NOT NULL,
  `ID` mediumint(8) UNSIGNED NOT NULL,
  `patient` varchar(50) DEFAULT NULL,
  `tissue` varchar(50) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE `eavs`
  ADD PRIMARY KEY (`id`);

ALTER TABLE `eavs`
  MODIFY `id` int(15) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=3707;
    
ALTER TABLE `sources`
  ADD PRIMARY KEY (`source_id`),
  ADD UNIQUE KEY `name` (`name`);

ALTER TABLE `sources`
  MODIFY `source_id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=15;

ALTER TABLE `uploaddatastatus`
  ADD PRIMARY KEY (`ID`);

ALTER TABLE `uploaddatastatus`
  MODIFY `ID` mediumint(8) UNSIGNED NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=12;


CREATE TABLE `upload_error` (
  `ID` mediumint(8) UNSIGNED NOT NULL,
  `error_id` mediumint(8) UNSIGNED NOT NULL,
  `message` varchar(500) NOT NULL,
  `error_code` int(5) NOT NULL,
  `source_id` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

ALTER TABLE `upload_error`
  ADD PRIMARY KEY (`ID`);

ALTER TABLE `upload_error`
  MODIFY `ID` mediumint(8) UNSIGNED NOT NULL AUTO_INCREMENT;

INSERT INTO sources(source_id, name) VALUES (1, 'test_src');
|]

create_tables :: ConnInfo -> Assertion
create_tables (SomeDBType (db_type :: DBType ty), user, pass, host, port, db) = 
  bracket
    (DB.connect user pass host port db) 
    DB.disconnect $
    \(con :: ty) -> case db_type of
      Postgres -> makeDatabaseSchemaPostgres con
      MySQL -> MySQL.executeMany_ con makeDatabaseSchemaMySQL >> return ()



[yesh1|
-- name:clear_eavs_jsonb
DELETE FROM eavs_jsonb;
|]

[yesh1|
-- name:prepare_Postgres_for_test
-- :fileName :: String
INSERT INTO uploaddatastatus("FileName","uploadStart", "Status", source_id, user_id) VALUES (:fileName, current_timestamp, '_', 1, 1);
|]


prepare_MySQL_for_test :: String -> MySQL.MySQLConn -> IO ()
prepare_MySQL_for_test fileName con = MySQL.execute con
  "INSERT INTO uploaddatastatus(FileName,uploadStart, Status, source_id, user_id) VALUES (?, now(), '_', 1, 1);"
  [MySQLText $ toS fileName] >> return ()


[yesh1|
-- name:selectAllFromEAVS_JSONB :: [(Value)]
-- :fileName :: String
SELECT CAST(data AS VARCHAR)
  FROM eavs_jsonb INNER JOIN uploaddatastatus ON eavs_jsonb."fileName" = uploaddatastatus."ID"
  WHERE uploaddatastatus."FileName" = :fileName
|]


testProcessFileWithDB :: ConnInfo -> String -> String -> ByteString -> [Value] -> Assertion
testProcessFileWithDB c@(SomeDBType Postgres, user, pass, host, port, db) file file_contents row_fun expected = do
  bracket (DB.connect @Connection user pass host port db) DB.disconnect (\con -> clear_eavs_jsonb con >> prepare_Postgres_for_test (takeFileName file) con)
  withUtf8 $ writeFile file file_contents
  r <- quickjsMultithreaded $ do
    _ <- eval_ $ "rowFun = function(row, header) { " <> row_fun <> " }"
    processFile
      rowFun
      (const [])
      file
      JSONFile
      (DBOutputOpt c $ SourceID 1)
      LogToDB
      (TerminateOnError False)
      (WriteCountToDB True)
  bracket (DB.connect @Connection user pass host port db) DB.disconnect $ \con -> do
    res <- selectAllFromEAVS_JSONB (takeFileName file) con
    res @?= expected
  removeFile file

  where
    rowFun row header = call "rowFun" [row, header]
testProcessFileWithDB c@(SomeDBType MySQL, user, pass, host, port, db) file file_contents row_fun expected = do
  bracket (DB.connect user pass host port db) DB.disconnect (\con -> prepare_MySQL_for_test (takeFileName file) con)
  withUtf8 $ writeFile file file_contents
  _ <- quickjsMultithreaded $ do
    _ <- eval_ $ "rowFun = function(row, header) { " <> row_fun <> " }"
    processFile
      rowFun
      (const [])
      file
      JSONFile
      (DBOutputOpt c $ SourceID 1)
      LogToDB
      (TerminateOnError False)
      (WriteCountToDB True)
  return ()
  where
    rowFun row header = call "rowFun" [row, header]

test_file_json_1 = [i|{
  "subject_id": 0,
  "name": "Sam",
  "age": 25,
  "heart_rate": [110, 98, 123],
  "blood_pressure": [{"sys":95, "dia": 65}, {"sys":105, "dia": 72}, {"sys":98, "dia": 68}]
}|]


test_file_json_2 = [i|[{
  "subject_id": 0,
  "name": "Sam",
  "age": 25,
  "heart_rate": [110, 98, 123],
  "blood_pressure": [{"sys":95, "dia": 65}, {"sys":105, "dia": 72}, {"sys":98, "dia": 68}]
}]|]


test_file_json_1_expected = [
    Object [
      ("age", Number 25.0),
      ("name", String "Sam"),
      ("heart_rate", Array [Number 110.0,Number 98.0,Number 123.0]),
      ("blood_pressure", Array [
          Object [("sys",Number 95.0),("dia",Number 65.0)]
        , Object [("sys",Number 105.0),("dia",Number 72.0)]
        , Object [("sys",Number 98.0),("dia",Number 68.0)]
        ]),
      ("subject_id",Number 0.0),
      ("i",Number 0.0)
    ]
  ]



testProcessFile :: String -> String -> IO ()
testProcessFile inFile rowFunFile = do
  settings <- withUtf8 $ try $ do
    txtSettings <- BSL.readFile rowFunFile
    case (decode txtSettings :: Maybe Settings) of
      Nothing -> throwM $ InternalError "Invalid Settings file"
      Just Settings{..} -> do
        case compileSchema jsonSchema of
          Left _ -> do
            throwM $ InternalError "Invalid schema"
          Right validator ->
            return (toS processFunction, libraryFunctions, validator, openAs)

  let 
    (row_fun, libraryFunctions, validator, openAs) = 
      case settings of
        Left (_ :: SomeException) -> ("return row;", Nothing, const [], Unspecified)
        Right res -> res
  


  _ <- quickjsMultithreaded $ do
    mapM_ loadLibrary libraryFunctions
    _ <- eval_ $ "rowFun = function(row, header) { " <> row_fun <> " }"
    res <- try $ processFile
      rowFun
      validator
      inFile
      (fileType openAs)
      JSONFileOutputOpt
      LogToConsole
      (TerminateOnError True)
      (WriteCountToDB True)
    case res of
      Left (e :: RowError) -> liftIO $ withUtf8 $ writeFile (inFile <.> "out.json") $ show e
      -- in case we want to output RowError as JSON?
      -- Left (e :: RowError) -> liftIO $ withUtf8 $ BSL.writeFile (inFile <.> "out.json") $ encode e
      Right _ -> pure ()
  return ()
  where
    rowFun row header = call "rowFun" [row, header]
    fileType openAs = fromSpecifiedFileType (tail $ takeExtension inFile) openAs


goldenTestsProcessFile :: IO TestTree
goldenTestsProcessFile = do
  inputFiles :: [String] <- getDirectoryContents "./test/Runtime/golden" >>= 
    return . sortBy (\a b -> compare (takeExtension a ++ a) (takeExtension b ++ b)). filter (\f -> 
         f /= "."
      && f /= ".."
      && f /= ".DS_Store"
      && not (strEndsWith f "out.json")
      && takeExtension f /= ".gold"
      && takeExtension f /= ".settings")

  return $ testGroup "processFile golden tests"
    [ goldenVsFile
        (show fileType ++ " - " ++ strCapitalize (strReplace "_" " " $ takeBaseName inputFile)) -- test name
        goldenFile -- golden file path
        outputFile
        (testProcessFile ("./test/Runtime/golden" </> inputFile) rowFunFile) -- action whose result is tested
    | inputFile <- inputFiles
    , let goldenFile = "./test/Runtime/golden" </> inputFile <.> "gold"
          outputFile = "./test/Runtime/golden" </> inputFile <.> "out.json"
          rowFunFile = "./test/Runtime/golden" </> inputFile <.> "settings"
          fileType = fromSpecifiedFileType (tail $ takeExtension inputFile) Unspecified
    ]


tests :: IO ([ConnInfo] -> TestTree)
tests = do
  goldenTests <- goldenTestsProcessFile
  return $ \c -> testGroup "Runtime" $
    [ testCase "call loadLibrary and evaluate 'Lib.helloWorld();'" load_lib
    , goldenTests
    ] ++ (concat $ flip map c $
      \config -> 
        [ 
          testGroup ("processFile DB tests - " ++ case config of { (SomeDBType Postgres, _, _, _, _, _) -> "Postgres" ; (SomeDBType MySQL, _, _, _, _, _) -> "MySQL" })
            [ testCase "connect to the DB and set up tables" $ create_tables config
            , after AllSucceed "connect to the DB and set up tables" $
                testCase "test the processFile function on a JSON file 1" $ 
                  testProcessFileWithDB config "./test_file_json_1.json" test_file_json_2 "return row;" test_file_json_1_expected
            , after AllSucceed "test the processFile function on a JSON file 1" $
                testCase "test the processFile function on a JSON file 2, expecting the same result" $ 
                  testProcessFileWithDB config "./test_file_json_2.json" test_file_json_1 "return row;" test_file_json_1_expected
            ]
        ]
    )