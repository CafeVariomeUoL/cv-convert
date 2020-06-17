{-#LANGUAGE QuasiQuotes, MultiParamTypeClasses #-}

module DB where
import Database.HDBC
import Database.YeshQL.HDBC
import Data.Aeson           (Value, encode)
import Data.Convertible.Base
import Data.String.Conv     (toS)
import Data.Text            (Text)
import Data.UUID            (UUID, toASCIIBytes)

instance Convertible Value SqlValue where
  safeConvert v = return $ SqlString $ toS $ encode v

instance Convertible UUID SqlValue where
  safeConvert v = return $ SqlByteString $ toASCIIBytes v


[yesh|
-- name:getFileID :: (Int)
-- :sourceID :: Int
-- :fileName :: String
SELECT "ID" FROM uploaddatastatus WHERE source_id = :sourceID AND "FileName" = :fileName
;;;


-- name:deleteSouceFromEAVSJSONB :: ()
-- :sourceID :: Int
DELETE FROM eavs_jsonb WHERE source_id = :sourceID
;;;


-- name:deleteSouceFromEAVSJSONBAttributesValues :: ()
-- :sourceID :: Int
DELETE FROM eavs_jsonb_attributes_values WHERE source_id = :sourceID
;;;

-- name:insertJSONBOverrideOnConflict :: ()
-- :sourceID :: Int
-- :file :: Int
-- :subjectID :: String
-- :jsonbData :: Value
INSERT INTO eavs_jsonb(source_id, "fileName", subject_id, data) 
  VALUES (:sourceID,:file,:subjectID,:jsonbData) 
  ON CONFLICT (source_id, subject_id) DO UPDATE SET data = :jsonbData
;;;


-- name:insertJSONBAttributesValuesMergeOnConflict :: ()
-- :sourceID :: Int
-- :attr :: Value
-- :values :: Value
INSERT INTO eavs_jsonb_attributes_values(source_id, attribute, "values") 
  VALUES (:sourceID,:attr,:values) 
  ON CONFLICT (source_id, attribute) DO UPDATE SET "values" = eavs_jsonb_attributes_values."values" || :values
;;;


-- name:cleanupJSONBAttributesValues :: (Int)
UPDATE eavs_jsonb_attributes_values SET "values" = (
  SELECT array_to_json(array(SELECT DISTINCT jsonb_array_elements(t1."values"))) 
    FROM eavs_jsonb_attributes_values t1 
    WHERE t1.id = eavs_jsonb_attributes_values.id
)
;;;



-- name:insertEAV :: ()
-- :uuid :: UUID
-- :sourceID :: String
-- :fileName :: Int
-- :subjectID :: String
-- :attr :: Text
-- :value :: Text
INSERT INTO eavs(uid, source_id, "fileName", subject_id, type, attribute, value, elastic)
	VALUES (:uuid, :sourceID, :fileName, :subjectID, 'attribute', :attr, :value, false);
|] 