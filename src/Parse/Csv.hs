module Parse.Csv(readCsvFile) where

import           Data.Char                  (ord)
import           Data.Text                  (Text)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Aeson                 as JSON
import           Data.Csv.Streaming         (Records(..), HasHeader(..), decode, decodeWith)
import           Data.Csv.Parser            (defaultDecodeOptions, decDelimiter)
import qualified Data.HashMap.Strict        as HM
import           Control.Monad.Catch        (MonadThrow(..))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Lens.Combinators   (FoldableWithIndex, TraversableWithIndex, FunctorWithIndex)
import           Parse.Utils
import           Runtime.Error

instance FoldableWithIndex Int Records
instance TraversableWithIndex Int Records
instance FunctorWithIndex Int Records

-- Some UTF-8 encoded files start with the BOM mark, see:
-- https://en.wikipedia.org/wiki/Byte_order_mark
-- This messes up stuff and is not needed, so we remove it.
dropBOM :: BS.ByteString -> BS.ByteString
dropBOM bs | BS.take 3 bs == BS.pack [0xEF,0xBB,0xBF] = BS.drop 3 bs
           | otherwise = bs


readCsvFile :: (MonadThrow m, MonadIO m) => FilePath -> m ([JSON.Value], Records (HM.HashMap Text JSON.Value))
readCsvFile inFile = readCsvFileWithDelimiter [',', ';'] inFile


-- Try parsing a CSV file with ',' as the delimiter. If we encounter only one header, it is likely that a different separator was used.
-- In that case, try parsing the file again with ';' instead. If we still get only one header, revert to default, i.e. parsing with ','.
readCsvFileWithDelimiter :: (MonadThrow m, MonadIO m) => [Char] -> FilePath -> m ([JSON.Value], Records (HM.HashMap Text JSON.Value))
readCsvFileWithDelimiter [] inFile = do
  rows <- liftIO $ decode NoHeader <$> dropBOM <$> BS.readFile inFile
  case rows of
    Cons (Right (h :: [Text])) rest -> let header = uniqueHeader h in 
      return (map (uncurry unifyHeader) $ zip header h, fmap (\r -> HM.fromList $ zip header $ map JSON.toJSON r) rest)
    Cons (Left e) _ -> throwM $ CSVParseError e
    Nil (Just e) _ -> throwM $ CSVParseError e
    Nil Nothing r -> return ([], Nil Nothing r)
readCsvFileWithDelimiter (delim:alts) inFile = do
  rows <- liftIO $ decodeWith defaultDecodeOptions{ decDelimiter = fromIntegral (ord delim) } NoHeader <$> dropBOM <$> BS.readFile inFile
  case rows of
    Cons (Right (h :: [Text])) rest -> 
      if length h < 2 then readCsvFileWithDelimiter alts inFile
      else let header = uniqueHeader h in 
        return (map (uncurry unifyHeader) $ zip header h, fmap (\r -> HM.fromList $ zip header $ map JSON.toJSON r) rest)
    Cons (Left e) _ -> throwM $ CSVParseError e
    Nil (Just e) _ -> throwM $ CSVParseError e
    Nil Nothing r -> return ([], Nil Nothing r)