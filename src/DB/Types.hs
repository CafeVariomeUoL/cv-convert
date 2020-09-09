{-#LANGUAGE QuasiQuotes, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module DB.Types(
  SourceID(..), SubjectID(..), FileID(..)
) where

newtype SourceID = SourceID {unSourceID :: Int} deriving (Read, Show, Eq)

newtype SubjectID = SubjectID {unSubjectID :: String}

newtype FileID = FileID Int
