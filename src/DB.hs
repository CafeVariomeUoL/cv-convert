{-#LANGUAGE QuasiQuotes, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module DB(
  SourceID(..), SubjectID(..), FileID,
  getFileID,

  insertJSONBOverrideOnConflict,
  deleteSouceFromEAVSJSONB,

  insertJSONBAttributesValuesMergeOnConflict,
  deleteSouceFromEAVSJSONBAttributesValues,
  cleanupJSONBAttributesValues,

  insertEAV,

  insertError, 
  clearErrors
) where
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


newtype SourceID = SourceID {unSourceID :: Int} deriving (Read, Show, Eq)

instance Convertible SourceID SqlValue where
  safeConvert (SourceID v) = safeConvert v


newtype SubjectID = SubjectID {unSubjectID :: String}

instance Convertible SubjectID SqlValue where
  safeConvert (SubjectID v) = safeConvert v


newtype FileID = FileID {unFileID :: Int} deriving newtype (Convertible SqlValue)

instance Convertible FileID SqlValue where
  safeConvert (FileID v) = safeConvert v


[yesh|
-- name:getFileID :: (FileID)
-- :sourceID :: SourceID
-- :fileName :: String
SELECT "ID" FROM uploaddatastatus WHERE source_id = :sourceID AND "FileName" = :fileName
;;;


-- name:insertJSONBOverrideOnConflict :: ()
-- :sourceID :: SourceID
-- :fileID :: FileID
-- :subjectID :: SubjectID
-- :jsonbData :: Value
INSERT INTO eavs_jsonb(source_id, "fileName", subject_id, data) 
  VALUES (:sourceID,:fileID,:subjectID,:jsonbData) 
  ON CONFLICT (source_id, subject_id) DO UPDATE SET data = :jsonbData
;;;


-- name:deleteSouceFromEAVSJSONB :: ()
-- :sourceID :: SourceID
DELETE FROM eavs_jsonb WHERE source_id = :sourceID
;;;


-- name:insertJSONBAttributesValuesMergeOnConflict :: ()
-- :sourceID :: SourceID
-- :attr :: Value
-- :values :: Value
INSERT INTO eavs_jsonb_attributes_values(source_id, attribute, "values") 
  VALUES (:sourceID,:attr,:values) 
  ON CONFLICT (source_id, attribute) DO UPDATE SET "values" = eavs_jsonb_attributes_values."values" || :values
;;;


-- name:deleteSouceFromEAVSJSONBAttributesValues :: ()
-- :sourceID :: SourceID
DELETE FROM eavs_jsonb_attributes_values WHERE source_id = :sourceID
;;;


-- name:cleanupJSONBAttributesValues :: (Int)
UPDATE eavs_jsonb_attributes_values SET "values" = (
  SELECT array_to_json(array(SELECT DISTINCT jsonb_array_elements(t1."values"))) 
    FROM eavs_jsonb_attributes_values t1 
    WHERE t1.id = eavs_jsonb_attributes_values.id
)
;;;


-- name:insertEAV_ :: ()
-- :uuid :: UUID
-- :sourceID :: String
-- :fileID :: FileID
-- :subjectID :: SubjectID
-- :attr :: Text
-- :value :: Text
INSERT INTO eavs(uid, source_id, "fileName", subject_id, type, attribute, value, elastic)
	VALUES (:uuid, :sourceID, :fileID, :subjectID, 'attribute', :attr, :value, false);
;;;


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

|] 

insertEAV :: IConnection conn =>
  UUID -> SourceID -> FileID -> SubjectID -> Text -> Text -> conn -> IO ()
insertEAV uuid (SourceID sID) fileID subjectID attr value = insertEAV_ uuid (show sID) fileID subjectID attr value