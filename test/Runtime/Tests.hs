{-# LANGUAGE QuasiQuotes #-}

module Runtime.Tests(tests) where
import           Test.Tasty              (TestTree, testGroup, after, DependencyType(..),)
import           Test.Tasty.HUnit        (testCase)
-- import           Test.Tasty.QuickCheck   (testProperty, QuickCheckTests(..), QuickCheckVerbose(..))
import           Test.HUnit              (Assertion, (@?=))
-- import qualified Test.QuickCheck         as QC
-- import qualified Test.QuickCheck.Monadic as QC
import           Data.Aeson              (Value(..))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Catch          (bracket)
-- import           Data.Text               (pack)
-- import qualified Data.HashMap.Strict            as HS
-- import qualified Data.Vector             as V
import           Data.String.Interpolate      (i)
import           Main.Utf8                    (withUtf8)
import           Database.HDBC.PostgreSQL.Pure(Config, connect)
import           Database.HDBC.Types          (IConnection(commit, disconnect))
import           Database.YeshQL.HDBC

import           Runtime
import           Quickjs



lib_js = [i|let Utils = {
  helloWorld: function() {return 2;}
}
export default Utils;|]


load_lib :: Assertion
load_lib = do
    withUtf8 $ writeFile "./test-lib.js" lib_js
    quickjs $ do
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
|]


create_tables :: Config -> Assertion
create_tables config = bracket
            (connect config) 
            (\con -> commit con >> disconnect con)
            makeDatabaseSchema 


[yesh1|
  -- name:prepare_DB_for_test_process_file
  -- @ddl
INSERT INTO sources(source_id, name) VALUES (1, 'test_src');
INSERT INTO uploaddatastatus("FileName","uploadStart", "Status", source_id, user_id) VALUES ('test_file', current_timestamp, '_', 1, 1);
|]


test_process_file :: Config -> Assertion
test_process_file config = do
  bracket (connect config) (\con -> commit con >> disconnect con) prepare_DB_for_test_process_file 
  -- quickjs $
    -- processFile (Just config) (Just $ SourceId 1) "return {...row, prop: 1};"


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
                testCase "test the processFile function" $ test_process_file config
            ]
        ]
      Nothing -> []