{-# LANGUAGE CPP, OverloadedStrings #-}

module Runtime.Utils where

import           Data.String (IsString)
import           Data.Aeson (Value(..), encode)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Vector                   as V
import Data.List (intersperse)
import Data.String.Conv (StringConv, toS)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

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