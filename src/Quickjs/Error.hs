{-# LANGUAGE ExistentialQuantification, DuplicateRecordFields, GeneralizedNewtypeDeriving #-}

module Quickjs.Error where
import           Control.Exception   (Exception(..), SomeException)
import           Data.Typeable       (cast)
import           Data.Text           (Text)
import           Data.String.Conv    (toS)
import           Type.Reflection     (Typeable)
import           GHC.Generics
import           Foreign.C.Types
import           Data.Aeson          (ToJSON(..))

import           Quickjs.Types
import           Runtime.Error


data SomeJSRuntimeException = forall e . (Exception e, ToJSON e) => SomeJSRuntimeException e deriving Typeable

instance Show SomeJSRuntimeException where
    show (SomeJSRuntimeException e) = show e

instance ToJSON SomeJSRuntimeException where
    toJSON (SomeJSRuntimeException e) = toJSON e

instance Exception SomeJSRuntimeException where
    toException = runtimeExceptionToException
    fromException = runtimeExceptionFromException


jsRuntimeExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
jsRuntimeExceptionToException = toException . SomeJSRuntimeException

jsRuntimeExceptionFromException :: Exception e => SomeException -> Maybe e
jsRuntimeExceptionFromException x = do
    SomeJSRuntimeException a <- fromException x
    cast a


newtype JSRuntimeException e = JSRuntimeException e 
  deriving Generic
  deriving newtype (Show, ToJSON)

instance (Show e, Typeable e, ToJSON e) => Exception (JSRuntimeException e)
  where
    toException = jsRuntimeExceptionToException
    fromException = jsRuntimeExceptionFromException



instance ToJSON CLong where
  toJSON cl = toJSON (fromIntegral cl :: Integer)

data UnknownJSTag = UnknownJSTag {raw_tag :: !CLong} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException UnknownJSTag)
  deriving Exception via (JSRuntimeException UnknownJSTag)

instance Show UnknownJSTag where
  show UnknownJSTag{..} = "Uknown JS tag: " ++ show raw_tag


data UnsupportedTypeTag = UnsupportedTypeTag {_tag :: JSTagEnum} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException UnsupportedTypeTag)
  deriving Exception via (JSRuntimeException UnsupportedTypeTag)


instance Show UnsupportedTypeTag where
  show UnsupportedTypeTag{..} = "Unsupported type tag: " ++ show _tag


data JSException = JSException {location :: Text, message :: Text} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException JSException)
  deriving Exception via (JSRuntimeException JSException)

instance Show JSException where
    show JSException{..} = "JS runtime threw an exception in " ++ toS location ++ ":\n=================\n" ++ toS message ++ "\n=================\n"



data JSValueUndefined = JSValueUndefined {value :: Text} 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException JSValueUndefined)
  deriving Exception via (JSRuntimeException JSValueUndefined)

instance Show JSValueUndefined where
  show JSValueUndefined{..} =  "The JS value '" ++ toS value ++ "' is undefined."


data JSValueIncorrectType = 
  JSValueIncorrectType {
    name :: Text
  , expected :: JSTypeEnum
  , found :: JSTypeEnum
  } 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException JSValueIncorrectType)
  deriving Exception via (JSRuntimeException JSValueIncorrectType)

instance Show JSValueIncorrectType where
  show JSValueIncorrectType{..} = "Type mismatch of the JS value '" ++ toS name ++ "'. Expected: " ++ show expected ++ ", found: " ++ show found


data InternalError = InternalError { message :: Text } 
  deriving (Generic, Typeable)
  deriving ToJSON via (RuntimeException InternalError)
  deriving Exception via (JSRuntimeException InternalError)

instance Show InternalError where
  show InternalError{..} = "Internal error occured:\n" ++ toS message
