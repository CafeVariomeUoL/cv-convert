{-# LANGUAGE QuasiQuotes, OverloadedLists #-}

module Runtime.Tests(tests) where
import           Test.Tasty              (TestTree, testGroup, after, DependencyType(..),)
import           Test.Tasty.HUnit        (testCase, assertBool)
-- import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=))
-- import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..), decode)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Catch     (bracket)
import           Data.Text               (Text)
import           Data.ByteString         (ByteString)
import           Data.String.Conv        (toS)

-- import qualified Data.HashMap.Strict            as HS
-- import qualified Data.Vector             as V
import           Data.String.Interpolate      (i)
import           Main.Utf8                    (withUtf8)
import           Database.HDBC.PostgreSQL.Pure(Config, connect)
import           Database.HDBC.Types          (IConnection(commit, disconnect))
import           Database.YeshQL.HDBC
import           System.FilePath.Posix        (takeFileName)

import           Runtime
import           Runtime.Error
import           Quickjs
import           DB



lib_js = [i|let Utils = {
  helloWorld: function() {return 2;}
}
export default Utils;|]


load_lib :: Assertion
load_lib = do
    withUtf8 $ writeFile "./test-lib.js" lib_js
    quickjsTest $ do
      loadLibrary "./test-lib.js"
      v <- eval "Utils.helloWorld();"
      liftIO $ v @?= Number 2



[yesh1|
-- name:makeDatabaseSchema
-- @ddl
CREATE TABLE sources (
    source_id serial primary key,
    name character varying(30) NOT NULL
);
CREATE TABLE eavs (
    id serial primary key,
    uid character varying(50) NOT NULL,
    source_id character varying(50) NOT NULL,
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


create_tables :: Config -> Assertion
create_tables config = 
  bracket
    (connect config) 
    (\con -> commit con >> disconnect con)
    makeDatabaseSchema


[yesh1|
-- name:prepare_DB_for_test
-- :fileName :: String
INSERT INTO uploaddatastatus("FileName","uploadStart", "Status", source_id, user_id) VALUES (:fileName, current_timestamp, '_', 1, 1);
|]


[yesh1|
-- name:selectAllFromEAVS_JSONB :: [(Value)]
-- :fileName :: String
SELECT CAST(data AS VARCHAR)
  FROM eavs_jsonb INNER JOIN uploaddatastatus ON eavs_jsonb."fileName" = uploaddatastatus."ID"
  WHERE uploaddatastatus."FileName" = :fileName
|]


test_process_file :: Config -> String -> String -> ByteString -> [Value] -> Assertion
test_process_file config file file_contents row_fun expected = do
  bracket (connect config) (\con -> commit con >> disconnect con) (prepare_DB_for_test (takeFileName file))
  withUtf8 $ writeFile file file_contents
  r <- quickjsTest $ do
    _ <- eval_ $ "rowFun = function(row, header) { " <> row_fun <> " }"
    processFile
      (Just config) 
      (Just $ SourceID 1) 
      rowFun
      (const [])
      file
      Nothing
      JSON
      LogToDb
      0
  bracket (connect config) (\con -> commit con >> disconnect con) $ \con -> do
    res <- selectAllFromEAVS_JSONB (takeFileName file) con
    res @?= expected

  where
    rowFun row header = call "rowFun" [row, header]


test_file_json_1 = [i|{
  "subject_id": 0,
  "name": "Sam",
  "age": 25,
  "heart_rate": [110, 98, 123],
  "blood_pressure": [{"sys":95, "dia": 65}, {"sys":105, "dia": 72}, {"sys":98, "dia": 68}]
}|]


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

tests :: Maybe Config -> TestTree
tests c = 
  testGroup "Runtime" $
    [ testCase "call loadLibrary and evaluate 'Utils.helloWorld();'" load_lib
    ] ++ case c of 
      Just config -> 
        [ 
          testGroup "Runtime - DB" 
            [ testCase "connect to the DB and set up tables" $ create_tables config
            , after AllSucceed "connect to the DB and set up tables" $
                testCase "test the processFile function on a JSON file" $ 
                  test_process_file config "./test_file_json_1.json" test_file_json_1 "return row;" test_file_json_1_expected
            ]
        ]
      Nothing -> []