{-#LANGUAGE QuasiQuotes, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module DB.Types(
  SourceID(..), SubjectID(..), FileID(..), User(..), Password(..), Host(..), Port(..), Database(..)
) where

newtype SourceID = SourceID {unSourceID :: Int} deriving (Read, Show, Eq)

newtype SubjectID = SubjectID {unSubjectID :: String}

newtype FileID = FileID Int

newtype User = User String deriving Show
newtype Password = Password String deriving Show
newtype Host = Host String  deriving Show
newtype Port = Port (Maybe String) deriving Show
newtype Database = Database String deriving Show
