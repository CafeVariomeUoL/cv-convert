{-# LANGUAGE CPP, OverloadedStrings #-}

module Runtime.Utils where

import           Data.String          (IsString)
import           Data.Aeson           (Value(..), encode)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Vector          as V
import           Data.List            (intersperse)
import           Data.String.Conv     (StringConv, toS)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           Data.Foldable        (foldlM)
import           Data.Csv.Streaming   (Records(..))

red, green, yellow, blue :: IsString a => a
red = "\ESC[031m"
green = "\ESC[032m"
yellow = "\ESC[033m"
blue = "\ESC[034m"

color :: (Semigroup a, IsString a) => a -> a -> a
#if defined(mingw32_HOST_OS)
color _ s = s
#else
color c s = c <> s <> "\ESC[0m"
#endif


intercalate :: (IsString a, Semigroup a) => a -> [a] -> a
intercalate s = foldr (<>) "" . intersperse s

colorJSON :: (IsString a, Semigroup a, StringConv ByteString a, StringConv Text a) => Value -> a
colorJSON (Object o) = "{" <> (intercalate ", " $ map (\(k,v) -> (color blue $ "\"" <> toS k <> "\"") <> ": " <> colorJSON v) $ HM.toList o) <> "}"
colorJSON (Array a) = "[" <> (intercalate ", " $ map colorJSON $ V.toList a) <> "]"
colorJSON s@(String _) = color blue $ toS $ encode s
colorJSON n@(Number _) = color green $ toS $ encode n
colorJSON rest = color yellow $ toS $ encode rest


class Foldable f => FoldableWithIndex f where
  ifoldM :: Monad m => (Int -> b -> a -> m b) -> b -> f a -> m b 


instance FoldableWithIndex [] where
  ifoldM f b as = foldlM (\b' (i,a) -> f i b' a) b (zip [0..] as)

instance FoldableWithIndex V.Vector where
  ifoldM f b as = V.ifoldM (\a i b' -> f i a b') b as


ifoldrRecords :: (Int -> a -> b -> b) -> b -> Records a -> b
ifoldrRecords f = go 0
  where
    go n z (Cons (Right x) rs) = f n x (go (n+1) z rs)
    go n z (Cons (Left _) rs) = go (n+1) z rs
    go _ z _ = z


instance FoldableWithIndex Records where
  ifoldM f z0 xs = ifoldrRecords c return xs z0
    where c n x k z = f n z x >>= k
          {-# INLINE c #-}
