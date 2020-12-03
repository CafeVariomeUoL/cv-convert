{-#LANGUAGE QuasiQuotes, DerivingStrategies, GeneralizedNewtypeDeriving #-}

{-|
Module      : DB.Postgres
Description : Database functions
Copyright   : (c) Samuel Balco, 2020
License     : MIT
Maintainer  : sam@definitelynotspam.email

/Note: Postgres backend functions./
-}
module DB.Postgres(
  -- * Inserting records into the database
  insertJSONBOverwriteOnConflict,
  insertEAV,
  -- * Inserting record metadata (only for JSONB) into the database
  insertJSONBAttributesValuesMergeOnConflict,
  cleanupJSONBAttributesValues,
  -- * Storing error log output
  insertError, 
  clearErrors,
  -- * Misc
  getFileID,
  deleteSourceFromEAVSJSONB,
  deleteSourceFromEAVSJSONBAttributesValues,
  updateRecordCount
) where
import Database.HDBC
import Database.YeshQL.HDBC
import Data.Aeson           (Value, encode, eitherDecode)
import Data.Convertible.Base
import Data.String.Conv     (toS)
import Data.Text            (Text)
import Data.ByteString.Lazy (ByteString)
import Data.UUID            (UUID, toASCIIBytes)

import DB.Types

instance Convertible Value SqlValue where
  safeConvert v = return $ SqlString $ toS $ encode v

instance Convertible SqlValue Value where
  safeConvert v = do 
    str :: ByteString <- safeConvert v
    case eitherDecode str of
      Left err -> convError err v
      Right res -> return res


instance Convertible UUID SqlValue where
  safeConvert v = return $ SqlByteString $ toASCIIBytes v


instance Convertible SourceID SqlValue where
  safeConvert (SourceID v) = safeConvert v


instance Convertible SubjectID SqlValue where
  safeConvert (SubjectID v) = safeConvert v


instance Convertible FileID SqlValue where
  safeConvert (FileID v) = safeConvert v

instance Convertible SqlValue FileID where
  safeConvert v = fmap FileID $ safeConvert v


{-|
Stores a row in the JSON 'Value' format (serialising into the JSONB type in Postres). 
If the 'SourceID' and 'SubjectID' alredy exist, the old record is overwritten.
-}
[yesh1|
-- name:insertJSONBOverwriteOnConflict :: ()
-- :sourceID :: SourceID
-- :fileID :: FileID
-- :subjectID :: SubjectID
-- :jsonbData :: Value
INSERT INTO eavs_jsonb(source_id, "fileName", subject_id, data) 
  VALUES (:sourceID,:fileID,:subjectID,:jsonbData) 
  ON CONFLICT (source_id, subject_id) DO UPDATE SET data = :jsonbData
|]


{-|
Stores a row in the EAV triples format. 

Given the 'Value'

>{
>  "subject_id": "patient0",
>  "age": 25.5,
>  "address": {
>    "street": "Middle",
>    "house_no": 26,
>    "city": "Towcester"
>  }
>}

we can generate the EAV tripples via 'flattenToEAV':

>(
>  60bcf653-fc8e-40dd-bb15-c332a99969da,
>  [
>    (60bcf653-fc8e-40dd-bb15-c332a99969da, "subject_id", "patient0"),
>    (60bcf653-fc8e-40dd-bb15-c332a99969da, "age", "25.5"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "parent_uuid", "60bcf653-fc8e-40dd-bb15-c332a99969da"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "parent_attr", "address"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "city", "Towcester"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "street", "Middle"),
>    (d53f0483-1107-4a8c-b544-02e8ed54778d, "house_no", "26")
>  ]
>)

These are then inserted into the @eavs@ table:

+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| uid                                  | source_id | fileName | subject_id | attribute   | value                                |
+======================================+===========+==========+============+=============+======================================+
| 60bcf653-fc8e-40dd-bb15-c332a99969da | ...       | ...      | patient0   | subject_id  | patient0                             |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| 60bcf653-fc8e-40dd-bb15-c332a99969da | ...       | ...      | patient0   | age         | 25.5                                 |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| d53f0483-1107-4a8c-b544-02e8ed54778d | ...       | ...      | patient0   | parent_uuid | 60bcf653-fc8e-40dd-bb15-c332a99969da |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| d53f0483-1107-4a8c-b544-02e8ed54778d | ...       | ...      | patient0   | parent_attr | address                              |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| d53f0483-1107-4a8c-b544-02e8ed54778d | ...       | ...      | patient0   | city        | Towcester                            |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| d53f0483-1107-4a8c-b544-02e8ed54778d | ...       | ...      | patient0   | street      | Middle                               |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+
| d53f0483-1107-4a8c-b544-02e8ed54778d | ...       | ...      | patient0   | house_no    | 26                                   |
+--------------------------------------+-----------+----------+------------+-------------+--------------------------------------+

-}
[yesh1|
-- name:insertEAV :: ()
-- :uuid :: UUID
-- :sourceID :: SourceID
-- :fileID :: FileID
-- :subjectID :: SubjectID
-- :attr :: Text
-- :value :: Text
INSERT INTO eavs(uid, source_id, "fileName", subject_id, type, attribute, value, elastic)
	VALUES (:uuid, :sourceID, :fileID, :subjectID, 'attribute', :attr, :value, false);
|]


{-|
The @eavs_jsonb_attributes_values@ table stores all the possible values, generated by 'createAllPathsWithValues' for all the inserted objects.
For example, given these two rows of data:

>[
>  {"gender": "female", "age": 27},
>  {"gender": "male", "age": 25}
>]

We generate and insert the following records into @eavs_jsonb_attributes_values@:

+-----------+---------------------+----------------------+
| source_id | attribute           | values               |
+===========+=====================+======================+
| ...       | @{"gender": "str"}@ | @["female", "male"]@ |
+-----------+---------------------+----------------------+
| ...       | @{"age": "int"}@    | @[27, 25]@           |
+-----------+---------------------+----------------------+

If we add a third record @r@ 

>{"gender": "unknown", "age": 35},

and insert the result 'createAllPathsWithValues' @r@ into  @eavs_jsonb_attributes_values@, 
the @values@ of both rows will get merged with the values already in the table:

+-----------+---------------------+---------------------------------+
| source_id | attribute           | values                          |
+===========+=====================+=================================+
| ...       | @{"gender": "str"}@ | @["female", "male", "unknown"]@ |
+-----------+---------------------+---------------------------------+
| ...       | @{"age": "int"}@    | @[27, 25, 35]@                  |
+-----------+---------------------+---------------------------------+

-}
[yesh1|
-- name:insertJSONBAttributesValuesMergeOnConflict :: ()
-- :sourceID :: SourceID
-- :attr :: Value
-- :values :: Value
INSERT INTO eavs_jsonb_attributes_values(source_id, attribute, "values") 
  VALUES (:sourceID,:attr,:values) 
  ON CONFLICT (source_id, attribute) DO UPDATE SET "values" = eavs_jsonb_attributes_values."values" || :values
|]


{-|
This function removes duplicates in the @values@ column of the @eavs_jsonb_attributes_values@ table.

Should be run after we have inserted all the records via 'insertJSONBAttributesValuesMergeOnConflict'.
-}
[yesh1|
-- name:cleanupJSONBAttributesValues :: (Int)
UPDATE eavs_jsonb_attributes_values SET "values" = (
  SELECT array_to_json(array(SELECT DISTINCT jsonb_array_elements(t1."values"))) 
    FROM eavs_jsonb_attributes_values t1 
    WHERE t1.id = eavs_jsonb_attributes_values.id
)
|]



[yesh|
-- name:insertError :: ()
-- :sourceID :: SourceID
-- :fileID :: FileID
-- :message :: String
INSERT INTO upload_error(source_id, error_id, message, error_code)
	VALUES (:sourceID, :fileID, :message, 0)
;;;


-- name:clearErrors :: ()
-- :sourceID :: SourceID
-- :fileID :: FileID
DELETE FROM upload_error
	WHERE source_id = :sourceID AND error_id = :fileID;
;;;


-- name:getFileID :: (FileID)
-- :sourceID :: SourceID
-- :fileName :: String
SELECT "ID" FROM uploaddatastatus WHERE source_id = :sourceID AND "FileName" = :fileName
;;;


-- name:deleteSourceFromEAVSJSONB :: ()
-- :sourceID :: SourceID
DELETE FROM eavs_jsonb WHERE source_id = :sourceID
;;;


-- name:deleteSourceFromEAVSJSONBAttributesValues :: ()
-- :sourceID :: SourceID
DELETE FROM eavs_jsonb_attributes_values WHERE source_id = :sourceID
;;;

-- name:updateRecordCount :: ()
-- :sourceID :: SourceID
-- :count :: Int
UPDATE sources SET record_count = record_count + :count WHERE source_id = :sourceID

|]