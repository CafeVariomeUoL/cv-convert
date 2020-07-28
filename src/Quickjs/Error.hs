{-# LANGUAGE ExistentialQuantification, DuplicateRecordFields #-}

module Quickjs.Error where
import           Control.Exception (Exception(..), SomeException)
import           Data.Typeable       (cast)
import           Data.Text           (Text)
import           Data.String.Conv    (toS)
import           Type.Reflection     (Typeable)
import           GHC.Generics
import           Foreign.C.Types
import           Data.Aeson          (Value(..), ToJSON(..))
import qualified Data.HashMap.Strict as HM

import           Quickjs.Types
import           Runtime.Error


data JSRuntimeException = forall e . (Exception e, ToJSON e) => JSRuntimeException e
    deriving Typeable

instance Show JSRuntimeException where
    show (JSRuntimeException e) = show e

instance ToJSON JSRuntimeException where
    toJSON (JSRuntimeException e) = toJSON e

instance Exception JSRuntimeException where
    toException = runtimeExceptionToException
    fromException = runtimeExceptionFromException


jsRuntimeExceptionToException :: (Exception e, ToJSON e) => e -> SomeException
jsRuntimeExceptionToException = toException . JSRuntimeException

jsRuntimeExceptionFromException :: Exception e => SomeException -> Maybe e
jsRuntimeExceptionFromException x = do
    JSRuntimeException a <- fromException x
    cast a


instance ToJSON CLong where
  toJSON cl = toJSON (fromIntegral cl :: Integer)

data UnknownJSTag = UnknownJSTag {raw_tag :: !CLong} deriving (Generic, Typeable)

instance Show UnknownJSTag where
  show UnknownJSTag{..} = "Uknown JS tag: " ++ show raw_tag

instance ToJSON UnknownJSTag where
  toJSON = genericExceptionToJSON

instance Exception UnknownJSTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data UnsupportedTypeTag = UnsupportedTypeTag {_tag :: JSTagEnum} deriving (Generic, Typeable)

instance Show UnsupportedTypeTag where
  show UnsupportedTypeTag{..} = "Unsupported type tag: " ++ show _tag

instance ToJSON UnsupportedTypeTag where
  toJSON UnsupportedTypeTag{..} = Object $ HM.fromList [("error_type", String "UnknownJSTag"), ("tag", toJSON _tag)]

instance Exception UnsupportedTypeTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSException = JSException {location :: Text, message :: Text} deriving (Generic, Typeable)

instance Show JSException where
    show JSException{..} = "JS runtime threw an exception in " ++ toS location ++ ":\n=================\n" ++ toS message ++ "\n=================\n"

instance ToJSON JSException where
  toJSON = genericExceptionToJSON

instance Exception JSException where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSValueUndefined = JSValueUndefined {value :: Text} deriving (Generic, Typeable)

instance Show JSValueUndefined where
  show JSValueUndefined{..} =  "The JS value '" ++ toS value ++ "' is undefined."

instance ToJSON JSValueUndefined where
  toJSON = genericExceptionToJSON

instance Exception JSValueUndefined where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSValueIncorrectType = 
  JSValueIncorrectType {
    name :: Text
  , expected :: JSTypeEnum
  , found :: JSTypeEnum
  } deriving (Generic, Typeable)

instance Show JSValueIncorrectType where
  show JSValueIncorrectType{..} = "Type mismatch of the JS value '" ++ toS name ++ "'. Expected: " ++ show expected ++ ", found: " ++ show found

instance ToJSON JSValueIncorrectType where
  toJSON = genericExceptionToJSON

instance Exception JSValueIncorrectType where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data InternalError = InternalError { message :: Text } deriving (Generic, Typeable)

instance Show InternalError where
  show InternalError{..} = "Internal error occured:\n" ++ toS message

instance ToJSON InternalError where
  toJSON = genericExceptionToJSON

instance Exception InternalError where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException