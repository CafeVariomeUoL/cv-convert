{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleContexts            #-}

module Parse.Csv(readCsvFile) where

import           Data.Text                  (Text)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Aeson                 as JSON
import           Data.Csv.Streaming         (Records(..), HasHeader(..), decode)
import qualified Data.HashMap.Strict        as HM
import           Data.String.Conv           (toS)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Lens.Combinators   (FoldableWithIndex, TraversableWithIndex, FunctorWithIndex)


instance FoldableWithIndex Int Records
instance TraversableWithIndex Int Records
instance FunctorWithIndex Int Records

readCsvFile :: (MonadError String m, MonadIO m) => FilePath -> m ([Text], Records (HM.HashMap Text JSON.Value))
readCsvFile inFile = do
  rows <- liftIO $ decode NoHeader <$> BS.readFile inFile
  case rows of
    Cons (Right (h :: [Text])) rest -> 
      let header = uniqueHeader h in return (header, fmap (\r -> HM.fromList $ zip header $ map JSON.toJSON r) rest)
    Cons (Left e) _ -> throwError e
    Nil (Just e) _ -> throwError e
    Nil Nothing r -> return ([], Nil Nothing r)



uniqueHeader :: [Text] -> [Text]
uniqueHeader = f HM.empty
  where
    f :: HM.HashMap Text Int -> [Text] -> [Text]
    f _ [] = []
    f hdrDups (h:hs)
      | h `HM.member` hdrDups = (h <> (toS $ show $ hdrDups HM.! h)) : f (HM.adjust (+1) h hdrDups) hs
      | otherwise = h : f (HM.insert h 1 hdrDups) hs