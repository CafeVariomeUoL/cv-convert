{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleInstances         #-}

module Parse.Xlsx (readXlsxFile) where

import           Codec.Xlsx
import           Codec.Xlsx.Formatted
import           Control.Applicative        ((<|>))
import           Control.Exception          (throw)
import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (evalState, put)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as L
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Control.Monad.IO.Class     (MonadIO, liftIO)

-- | Extract a YAML file from an XLSX thingo
-- The first row of a sheet should have field names
-- data rows must have a key in the first column
type Row k v = M.Map k v
type Sheet r c v = M.Map (r, c) v



readXlsxFile :: MonadIO m => FilePath -> m [JSON.Value]
readXlsxFile inFile = do
    Xlsx{..} <- liftIO $ toXlsx <$> L.readFile inFile
    return $ concat $ fmap (sheetToValue 2 . snd) _xlSheets


-- used in testing
-- readSheet :: Int -> FilePath -> Text -> ExceptT T.Text IO (T.Text, YAML.Value)
-- readSheet dataStartRow inFile sheetToExtract =
--      do (ss, styles) <- liftIO $ readXlsxFile inFile
--         readSheet' dataStartRow styles ss sheetToExtract

-- readSheet
--   :: Monad m
--   => Int -> Xlsx -> Text -> ExceptT Text m JSON.Value
-- readSheet dataStartRow ss sheetToExtract =
--     case ss ^? ixSheet sheetToExtract of
--        Just x -> pure (sheetToValue dataStartRow x)
--        Nothing -> throwE sheetToExtract

-- | Encode a worksheet as a JSON Object.
-- First row is fields. Data rows start at R. Stop when you encounter Y blank rows
sheetToValue
  :: Int -> Worksheet -> [JSON.Value]
sheetToValue dataStart sheet =
  let fields = readFieldNames . extractRow 1 . _wsCells $ sheet
      dataRows = extractDefinedRows dataStart (M.mapMaybe _cellValue $ _wsCells sheet)
  in map (\(i,r) -> JSON.object $ rowToJSON fields "i" i r) $ zip [0..] dataRows

-- | Merge field names with a row of cells, into the contents of a JSON Object
rowToJSON
  :: (Ord k1, Integral i)
  => Row k1 k -> k -> i -> Row k1 CellValue -> [(k, JSON.Value)]
rowToJSON fields i_k i row = (i_k , JSON.Number $ fromIntegral i) : (M.elems $ M.intersectionWith f fields row)
  where f fieldName value = (fieldName, cellValueToValue value)

readFieldNames :: Row k Cell -> Row k Text
readFieldNames cellsX =
  M.mapMaybe (^? cellValue . _Just . _CellText) cellsX

extractRow :: (Ord k2, Eq a1) => a1 -> Sheet a1 k2 a -> Row k2 a
-- extractRow :: (Ord k2, Eq a1) => a1 -> M.Map (a1, k2) a -> M.Map k2 a
extractRow selectedRow =
  M.mapKeys snd . M.filterWithKey (\rc _ -> fst rc == selectedRow)

-- Defined Rows are rows which have a value in the first column
-- Once enough (10) rows that are not defined have been found, we
-- stop searching
extractDefinedRows
  :: (Ord c, Num r, Num c, Eq r)
  => r -> M.Map (r, c) x -> [M.Map c x]
extractDefinedRows startRow sheet =
  evalState (action startRow) 0
  where
    stopWhenBadsIs = 100 :: Int -- will stop extracting rows when we encounter this many blanks in a row
    action rowNum = do
      let row = extractRow rowNum sheet
          firstCell = row ^? ix 1
      case firstCell of
        Nothing -> do
          numBads <- id <%= succ
          if numBads > stopWhenBadsIs
            then pure []
            else action (rowNum + 1)
        Just _ -> do
          put 0 -- reset the empty row counter
          rest <- action (rowNum + 1)
          pure (row : rest)


cellValueToValue :: CellValue -> JSON.Value
cellValueToValue (CellText t) = JSON.String t
cellValueToValue (CellDouble d) = JSON.Number (fromRational . toRational $ d)
cellValueToValue (CellBool b) = JSON.Bool b
cellValueToValue (CellRich b) = JSON.String (b ^. traverse . richTextRunText)

-- | Supporting Things

_CellText :: Prism' CellValue T.Text
_CellText = prism' CellText g
 where g (CellText t) = Just t
       g _ = Nothing

_CellDouble :: Prism' CellValue Double
_CellDouble  = prism' CellDouble g
 where g (CellDouble t) = Just t
       g _ = Nothing

_CellBool :: Prism' CellValue Bool
_CellBool = prism' CellBool g
 where g (CellBool t) = Just t
       g _ = Nothing

_CellRich :: Prism' CellValue [RichTextRun]
_CellRich = prism' CellRich g
 where g (CellRich t) = Just t
       g _ = Nothing