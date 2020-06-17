{-# LANGUAGE OverloadedStrings #-}

module JSON.Utils where

import           Data.Aeson            (Value(..), encode)
import           Data.Text             (Text)
import           Data.String.Conv      (toS)
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as S
import qualified Data.Vector           as V
import           Data.Scientific       (isInteger)
import           Data.UUID             as UUID
import           Data.UUID.V4          as UUID
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.Foldable         (foldlM)

createAllPathsWithValues :: Value -> HM.HashMap Value (S.HashSet Value)
createAllPathsWithValues (Object o) = 
  HM.foldlWithKey' (\acc k v -> 
      HM.unionWith
        (S.union)
        (HM.foldlWithKey' (\acc' k' v' -> HM.insert (Object $ HM.fromList [(k,k')]) v' acc') HM.empty $ createAllPathsWithValues v)
        acc
    ) HM.empty o
createAllPathsWithValues (Array a) = V.foldl' (\acc v -> 
      HM.unionWith
        (S.union)
        (createAllPathsWithValues v)
        acc
    ) HM.empty a
createAllPathsWithValues o@(String _) = HM.singleton (String "str") (S.singleton o)
createAllPathsWithValues o@(Number n)
  | isInteger n = HM.singleton (String "int") (S.singleton o)
  | otherwise = HM.singleton (String "float") (S.singleton o)
createAllPathsWithValues o@(Bool _) = HM.singleton (String "bool") (S.singleton o)
createAllPathsWithValues Null = HM.singleton (String "null") (S.singleton Null)



flattenToEAV :: MonadIO m => Value -> m (UUID.UUID, [(UUID.UUID, Text, Text)])
flattenToEAV (Object o) = do
  uuid <- liftIO $ UUID.nextRandom
  res <- foldlM (\acc (attr,val) -> do
      (_,r) <- f uuid attr val
      return $ r ++ acc
    ) [] $ HM.toList o
  return $ (uuid, res)
  where
    f :: MonadIO m => UUID.UUID -> Text -> Value -> m (UUID.UUID, [(UUID.UUID, Text, Text)])
    f uuid attr val@(Object _) = do
      (uuid', res) <- flattenToEAV val
      return $ (uuid, (uuid', "parent_uuid", UUID.toText uuid) : (uuid', "parent_attr", attr) : res)
    f uuid attr (Array as) = do
      res <- foldlM (\acc v -> do
          (_,r) <- f uuid attr v 
          return $ r ++ acc
        ) [] as
      return (uuid, res)
    f uuid attr (String val) = pure (uuid, [(uuid, attr, val)])
    f uuid attr val = pure (uuid, [(uuid, attr, toS $ encode val)])
flattenToEAV _ = undefined