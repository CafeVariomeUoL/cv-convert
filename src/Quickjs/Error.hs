{-# LANGUAGE ExistentialQuantification #-}

module Quickjs.Error where
import           Control.Exception (Exception(..), SomeException)
import           Data.Typeable     (cast)
import           Type.Reflection   (Typeable)
import           GHC.Generics
import           Foreign.C.Types
import           Data.Aeson        (Value(..), ToJSON(..))
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
  show (UnknownJSTag t) = "Uknown JS tag: " ++ show t

instance ToJSON UnknownJSTag where
  toJSON = genericExceptionToJSON

instance Exception UnknownJSTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data UnsupportedTypeTag = UnsupportedTypeTag {_tag :: JSTagEnum} deriving (Generic, Typeable)

instance Show UnsupportedTypeTag where
  show (UnsupportedTypeTag t) = "Unsupported type tag: " ++ show t

instance ToJSON UnsupportedTypeTag where
  toJSON UnsupportedTypeTag{..} = Object $ HM.fromList [("error_type", String "UnknownJSTag"), ("tag", toJSON _tag)]

instance Exception UnsupportedTypeTag where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSException = JSException {message :: String} deriving (Generic, Typeable)

instance Show JSException where
    show (JSException err) = "JS runtime threw an exception:\n" ++ err

instance ToJSON JSException where
  toJSON = genericExceptionToJSON

instance Exception JSException where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSValueUndefined = JSValueUndefined {value :: String} deriving (Generic, Typeable)

instance Show JSValueUndefined where
  show (JSValueUndefined v) =  "The JS value '" ++ v ++ "' is undefined."

instance ToJSON JSValueUndefined where
  toJSON = genericExceptionToJSON

instance Exception JSValueUndefined where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data JSValueIncorrectType = 
  JSValueIncorrectType {
    name :: String
  , expected :: JSTypeEnum
  , found :: JSTypeEnum
  } deriving (Generic, Typeable)

instance Show JSValueIncorrectType where
  show JSValueIncorrectType{..} = "Type mismatch of the JS value '" ++ name ++ "'. Expected: " ++ show expected ++ ", found: " ++ show found

instance ToJSON JSValueIncorrectType where
  toJSON = genericExceptionToJSON

instance Exception JSValueIncorrectType where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException


data InternalError = InternalError { message :: String } deriving (Generic, Typeable)

instance Show InternalError where
  show (InternalError err) = "Internal error occured:\n" ++ err

instance ToJSON InternalError where
  toJSON = genericExceptionToJSON

instance Exception InternalError where
  toException   = jsRuntimeExceptionToException
  fromException = jsRuntimeExceptionFromException