module Parse.Utils where

import qualified Data.Aeson                 as JSON
import           Data.Text                  (Text)
import qualified Data.Map.Strict            as M
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (mapAccumL)
import           Data.String.Conv           (StringConv, toS)

-- | we have to make sure that non-unique columns are distinguished. we do this by appending a number to every occurence of the same column name
uniqueHeader :: (Traversable t, Ord a, Semigroup a, StringConv String a) => t a -> t a
uniqueHeader = snd . mapAccumL f M.empty 
  where 
    f :: (Semigroup a, Ord a, StringConv String a) => M.Map a Int -> a -> (M.Map a Int, a)
    f hdrDups h
      | h `M.member` hdrDups = (M.adjust (+1) h hdrDups , h <> (toS $ show $ hdrDups M.! h))
      | otherwise = (M.insert h 1 hdrDups , h)


unifyHeader :: Text -> Text -> JSON.Value
unifyHeader u o
  | u == o = JSON.toJSON $ HM.fromList ([("h", o)] :: [(Text, Text)])
  | otherwise = JSON.toJSON $ HM.fromList ([("h", u), ("original", o)] :: [(Text, Text)])