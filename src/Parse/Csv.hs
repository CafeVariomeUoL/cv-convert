{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE OverloadedStrings           #-}

module Parse.Csv(readCsvFile) where

import           Data.Text                  (Text)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.Aeson                 as JSON
import           Data.Csv.Streaming         (Records(..), HasHeader(..), decode)
import qualified Data.HashMap.Strict        as HM
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Lens.Combinators   (FoldableWithIndex, TraversableWithIndex, FunctorWithIndex)
import           Parse.Utils

instance FoldableWithIndex Int Records
instance TraversableWithIndex Int Records
instance FunctorWithIndex Int Records

readCsvFile :: (MonadError String m, MonadIO m) => FilePath -> m ([JSON.Value], Records (HM.HashMap Text JSON.Value))
readCsvFile inFile = do
  rows <- liftIO $ decode NoHeader <$> BS.readFile inFile
  case rows of
    Cons (Right (h :: [Text])) rest -> 
      let header = uniqueHeader h in return (map (uncurry unifyHeader) $ zip header h, fmap (\r -> HM.fromList $ zip header $ map JSON.toJSON r) rest)
    Cons (Left e) _ -> throwError e
    Nil (Just e) _ -> throwError e
    Nil Nothing r -> return ([], Nil Nothing r)
