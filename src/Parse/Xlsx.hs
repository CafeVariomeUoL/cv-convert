{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE FlexibleInstances         #-}

module Parse.Xlsx (readXlsxFile) where

import           Codec.Xlsx
import           Control.Lens
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State        (evalState, put)
import qualified Data.Aeson                 as JSON
import qualified Data.ByteString.Lazy       as L
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Control.Monad.IO.Class     (MonadIO)
import           Data.List                  (sortOn)
import           Data.String.Conv           (StringConv, toS)

-- Adapted from https://github.com/nkpart/xlsx2yaml/blob/master/src/Lib.hs

type Row k v = M.Map k v
type Sheet r c v = M.Map (r, c) v



readXlsxFile :: MonadIO m => FilePath -> m [([Text], [JSON.Value])]
readXlsxFile inFile = do
    Xlsx{..} <- liftIO $ toXlsx <$> L.readFile inFile
    return $ fmap (sheetToValue 2 . snd) _xlSheets


-- | Encode a worksheet as a JSON Object.
sheetToValue
  :: Int -> Worksheet -> ([Text],[JSON.Value])
sheetToValue dataStart sheet =
  let fields = uniqueHeader . readFieldNames . extractRow 1 . _wsCells $ sheet
      dataRows = extractDefinedRows dataStart (M.mapMaybe _cellValue $ _wsCells sheet)
      rows = map (\(i,r) -> JSON.object $ rowToJSON fields "i" i r) $ zip [0..] dataRows
  in (map snd $ sortOn fst $ M.toList $ fields, rows)


-- | we have to make sure that non-unique columns are distinguished. we do this by appending a number to every occurence of the same column name
uniqueHeader :: (Ord k, Ord a, Semigroup a, StringConv String a) =>
                      M.Map k a -> M.Map k a
uniqueHeader = M.fromList . f M.empty . sortOn fst .  M.toList
  where 
    f :: (Semigroup a, Ord a, StringConv String a) => M.Map a Int -> [(k,a)] -> [(k,a)]
    f _ [] = []
    f hdrDups ((i,h):hs)
      | h `M.member` hdrDups = (i,h <> (toS $ show $ hdrDups M.! h)) : f (M.adjust (+1) h hdrDups) hs
      | otherwise = (i,h) : f (M.insert h 1 hdrDups) hs

-- | Merge field names with a row of cells, into the contents of a JSON Object
rowToJSON
  :: Ord k1
  => Row k1 Text -> Text -> Int -> Row k1 CellValue -> [(Text, JSON.Value)]
rowToJSON fields i_k i row = (i_k , JSON.Number $ fromIntegral i) : (M.elems $ M.intersectionWith f fields row)
  where f fieldName value = (fieldName, cellValueToValue value)

readFieldNames :: Row k Cell -> Row k Text
readFieldNames cellsX =
  M.mapMaybe (^? cellValue . _Just . _CellText) cellsX

extractRow :: (Ord k2, Eq a1) => a1 -> Sheet a1 k2 a -> Row k2 a
extractRow selectedRow =
  M.mapKeys snd . M.filterWithKey (\rc _ -> fst rc == selectedRow)

-- Defined Rows are rows which have a value in the first column
-- Once enough (100) rows that are not defined have been found, we
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
cellValueToValue (CellError _) = JSON.Null

-- | Supporting Things

_CellText :: Prism' CellValue T.Text
_CellText = prism' CellText g
 where g (CellText t) = Just t
       g _ = Nothing