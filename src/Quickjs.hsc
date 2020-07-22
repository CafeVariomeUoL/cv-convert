{-# LANGUAGE BangPatterns, QuasiQuotes, TemplateHaskell #-}

module Quickjs (JSValue, JSEvalType(..), JSContextPtr, quickjs, call_, eval, withJSValue) where

import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import Control.Monad.Catch         (MonadThrow(..), MonadCatch(..), MonadMask(..), finally)
import Control.Monad               (when, forM_)
import Control.Monad.Reader        (MonadReader, runReaderT, ask)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Data.Aeson                  (Value(..))
import qualified Data.Aeson        as Aeson
import Data.Scientific             (fromFloatDigits, toRealFloat, toBoundedInteger, isInteger)
import Data.Text                   (Text, pack, unpack)
import Data.Vector                 (fromList, imapM_)
import Data.HashMap.Strict         (HashMap, empty, insert, toList)
import Data.String.Conv            (toS)

import Quickjs.Types
import Quickjs.Error


C.context quickjsCtx
C.include "quickjs.h"
C.include "quickjs-libc.h"


foreign import ccall "JS_NewRuntime"
  jsNewRuntime :: IO (Ptr JSRuntime)

foreign import ccall "JS_FreeRuntime"
  jsFreeRuntime :: Ptr JSRuntime -> IO ()



foreign import ccall "JS_NewContext"
  jsNewContext :: Ptr JSRuntime -> IO (Ptr JSContext)

foreign import ccall "JS_FreeContext"
  jsFreeContext :: Ptr JSContext -> IO ()



jsFreeValue :: JSContextPtr -> JSValue -> IO ()
jsFreeValue ctx val = with val $ \v -> [C.block| void {
    if (JS_VALUE_HAS_REF_COUNT(*$(JSValue *v))) {
      JSRefCountHeader *p = (JSRefCountHeader *)JS_VALUE_GET_PTR(*$(JSValue *v));
      if (--p->ref_count <= 0) {
        __JS_FreeValue($(JSContext *ctx), *$(JSValue *v));
      }
    }
  } |]



type JSContextPtr = Ptr JSContext
type JSValueConstPtr = Ptr JSValueConst

jsIs_ :: (MonadIO m, Storable p, Eq n, Num n) => p -> (Ptr p -> IO n) -> m Bool
jsIs_ val fun = do
  b <- liftIO $ with val fun
  return $ b == 1

jsIsException :: MonadIO m => JSValue -> m Bool
jsIsException val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsException(*$(JSValueConst *valPtr)); } |]

jsIsNumber :: MonadIO m => JSValue -> m Bool
jsIsNumber val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsNumber(*$(JSValueConst *valPtr)); } |]

jsIsArray :: MonadIO m => JSContextPtr -> JSValue -> m Bool
jsIsArray ctxPtr val = jsIs_ val $ \valPtr -> [C.block| int { return JS_IsArray($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]


jsIsTryAll_ :: MonadThrow m =>
  JSValue -> [JSValue -> m Bool] -> [JSTypeEnum] -> m JSTypeEnum
jsIsTryAll_ jsval [] _ = case fromCType $ tag jsval of
  Just t -> return $ JSTypeFromTag t
  Nothing -> throwM $ UnknownJSTag (tag jsval)
jsIsTryAll_ jsval (f:funs)(l:lbls) = do
  b <- f jsval
  if b then return l else jsIsTryAll_ jsval funs lbls
jsIsTryAll_ _ _ _ = throwM $ InternalError $ "jsIsTryAll_ unreachable case"


jsIs :: (MonadIO m, MonadThrow m) => JSContextPtr -> JSValue -> m JSTypeEnum
jsIs ctx jsval = jsIsTryAll_ jsval [jsIsNumber, jsIsArray ctx] [JSIsNumber, JSIsArray]


jsNullValue :: JSValue
jsNullValue = JSValue { u = 0, tag = toCType JSTagNull }

jsNewBool :: JSContextPtr -> Bool -> IO JSValue
jsNewBool ctxPtr bool = do
  let b = if bool then 1 else 0
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewBool($(JSContext *ctxPtr), $(int b)); } |]

jsNewFloat64 :: JSContextPtr -> CDouble -> IO JSValue
jsNewFloat64 ctxPtr num =
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewFloat64($(JSContext *ctxPtr), $(double num)); } |]

jsNewInt64 :: JSContextPtr -> Int64 -> IO JSValue
jsNewInt64 ctxPtr num = do
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewInt64($(JSContext *ctxPtr), $(int64_t num)); } |]

jsNewString :: JSContextPtr -> String -> IO JSValue
jsNewString ctxPtr s = C.withPtr_ $ \ptr -> withCStringLen s $ \(cstringPtr, cstringLen) -> do
  let len = fromIntegral cstringLen
  [C.block| void { *$(JSValue *ptr) = JS_NewStringLen($(JSContext *ctxPtr), $(const char *cstringPtr), $(size_t len)); } |]



-- withJSValue :: (MonadThrow m, MonadIO m) => JSContext -> JSValue -> (Ptr JSValue -> m b) -> m b
-- withJSValue ctx json f = do
--     ptr <- liftIO malloc
--     jsonToJSValue ctx ptr json 
--     r <- f ptr `catchError` (\e -> do { cleanup ptr ; throwError e} )
--     cleanup ptr
--     return r
--   where
--     cleanup ptr = liftIO $ do
--       jsFreeValue ctx ptr
--       free ptr



checkIsExpcetion :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m ()
checkIsExpcetion ctxPtr val = do
  isExn <- jsIsException val
  when isExn $ do
    err <- getErrorMessage ctxPtr 
    liftIO $ jsFreeValue ctxPtr val
    throwM $ JSException err



jsonToJSValue :: (MonadThrow m, MonadIO m) => JSContextPtr -> Value -> m JSValue
jsonToJSValue _ Null = pure jsNullValue
jsonToJSValue ctx (Bool b) = liftIO $ jsNewBool ctx b
jsonToJSValue ctx (Number n) = 
  if not (isInteger n) then liftIO $ jsNewFloat64 ctx (toRealFloat n)
  else case toBoundedInteger n of
    Just i -> liftIO $ jsNewInt64 ctx i
    Nothing -> throwM $ InternalError "Value does not fit in Int64"
jsonToJSValue ctx (String s) = liftIO $ jsNewString ctx (unpack s) 
jsonToJSValue ctxPtr (Array xs) = do
  arrVal <- liftIO (C.withPtr_ $ \arrValPtr -> [C.block| void { *$(JSValueConst *arrValPtr) = JS_NewArray($(JSContext *ctxPtr)); } |])
  
  checkIsExpcetion ctxPtr arrVal

  flip imapM_ xs $ \index value -> do 
    val <- jsonToJSValue ctxPtr value
    checkIsExpcetion ctxPtr val

    let idx = fromIntegral index
    code <- liftIO (with arrVal $ \arrValPtr -> with val $ \valPtr -> 
      [C.block| int { return JS_DefinePropertyValueUint32(
        $(JSContext *ctxPtr), 
        *$(JSValueConst *arrValPtr),
        $(uint32_t idx),
        *$(JSValueConst *valPtr),
        JS_PROP_C_W_E
      ); } |])
    return ()

    if (code < 0) then do
      liftIO $ jsFreeValue ctxPtr arrVal
      throwM $ InternalError "Could not append element to array"
    else return ()

  return arrVal
jsonToJSValue ctxPtr (Object o) = do
  objVal <- liftIO (C.withPtr_ $ \objValPtr -> 
    [C.block| void { *$(JSValueConst *objValPtr) = JS_NewObject($(JSContext *ctxPtr)); } |])

  checkIsExpcetion ctxPtr objVal
  
  forM_ (toList o) $ \(key,value) -> do
    val <- jsonToJSValue ctxPtr value
    checkIsExpcetion ctxPtr val

    code <- liftIO (with objVal $ \objValPtr -> with val $ \valPtr -> 
      withCString (toS key) $ \cstringPtr -> do
        [C.block| int { return JS_DefinePropertyValueStr(
            $(JSContext *ctxPtr), 
            *$(JSValueConst *objValPtr),
            $(const char *cstringPtr),
            *$(JSValueConst *valPtr),
            JS_PROP_C_W_E
          ); } |])

    when (code < 0) $ do
      liftIO $ jsFreeValue ctxPtr objVal
      throwM $ InternalError "Could not add add property to object"

  return objVal


jsToBool :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Bool
jsToBool ctxPtr val = do
    code <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_ToBool($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    case code of
        -1 -> getErrorMessage ctxPtr >>= throwM . JSException
        0 -> return False
        _ -> return True

jsToInt64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m Int64
jsToInt64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \intPtr -> with val $ \valPtr -> [C.block| int { return JS_ToInt64($(JSContext *ctxPtr), $(int64_t *intPtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwM . JSException


jsToFloat64 :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> m CDouble
jsToFloat64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \doublePtr -> with val $ \valPtr -> [C.block| int { return JS_ToFloat64($(JSContext *ctxPtr), $(double *doublePtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwM . JSException



jsToString :: MonadIO m => JSContextPtr -> JSValue -> m String
jsToString ctxPtr val = liftIO $ do
    cstring <- with val $ \valPtr -> [C.block| const char * { return JS_ToCString($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    if cstring == nullPtr then return ""
    else do
      string <- peekCString cstring
      jsFreeCString ctxPtr cstring
      return string


foreign import ccall "JS_FreeCString"
  jsFreeCString :: JSContextPtr -> CString -> IO ()


jsToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m Value
jsToJSON ctx jsval = do
  ty <- jsIs ctx jsval
  case ty of
    JSTypeFromTag JSTagException -> do
      err <- getErrorMessage ctx 
      liftIO $ jsFreeValue ctx jsval
      throwM $ JSException err
    JSTypeFromTag JSTagNull -> return Null
    JSTypeFromTag JSTagUndefined -> return Null
    JSTypeFromTag JSTagBool -> do
      b <- jsToBool ctx jsval
      return $ Bool b
    JSIsNumber -> do
      n <- jsToFloat64 ctx jsval
      return $ Number $ fromFloatDigits n
    JSTypeFromTag JSTagString -> do
      s <- jsToString ctx jsval
      return $ String $ pack s
    JSIsArray -> do
      len <- do
        lenVal <- jsGetPropertyStr ctx jsval "length" 
        len' <- jsToInt64 ctx lenVal
        liftIO $ jsFreeValue ctx lenVal
        return len'
      vs <- jsArrayToJSON ctx jsval 0 (fromIntegral len)
      return $ Array $ fromList vs
    JSTypeFromTag JSTagObject -> do
      o <- jsObjectToJSON ctx jsval
      return $ Object o 
    JSTypeFromTag f -> throwM $ UnsupportedTypeTag f
    JSIsError -> throwM $ InternalError "JSIsError unreachable"


jsArrayToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> Int -> Int -> m [Value]
jsArrayToJSON ctxPtr jsval index len = 
  if index < len then do
    v <- do
      let idx = fromIntegral index
      val <- liftIO $ C.withPtr_ $ \ptr -> with jsval $ \jsvalPtr -> 
        [C.block| void { *$(JSValue *ptr) = JS_GetPropertyUint32($(JSContext *ctxPtr), *$(JSValueConst *jsvalPtr), $(uint32_t idx)); } |]

      checkIsExpcetion ctxPtr val
      res <- jsToJSON ctxPtr val
      liftIO $ jsFreeValue ctxPtr val
      return res

    vs <- jsArrayToJSON ctxPtr jsval (index+1) len
    return $ v:vs
  else return []



forLoop :: (Num a, Ord a, Monad m) => a -> (a -> m ()) -> m ()
forLoop end f = go 0
  where
    go !x | x < end   = f x >> go (x+1)
          | otherwise = return ()




jsObjectToJSON :: (MonadCatch m, MonadIO m) => JSContextPtr -> JSValue -> m (HashMap Text Value)
jsObjectToJSON ctxPtr obj = do
    let flags = toCType JSGPNStringMask .|. toCType JSGPNSymbolMask .|. toCType JSGPNEnumOnly
    properties <- liftIO $ malloc
    plen <- jsGetOwnPropertyNames ctxPtr obj properties flags 
      `catch` (\(e::JSRuntimeException) -> do
        liftIO $ free properties
        throwM e
      )
    objPtr <- liftIO $ malloc
    liftIO $ poke objPtr obj

    res <- collectVals properties objPtr 0 plen `catch` (\(e::JSRuntimeException) -> do
        liftIO $ free objPtr
        throwM e
      )
    cleanup properties plen
    return res
  where
    collectVals :: (MonadCatch m, MonadIO m) => Ptr (Ptr JSPropertyEnum) -> JSValueConstPtr -> Int -> Int -> m (HashMap Text Value)
    collectVals properties objPtr !index end 
      | index < end = do
        let i = fromIntegral index

        key <- do
          key' <- liftIO $ C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_AtomToString($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
          checkIsExpcetion ctxPtr key'
          res <- jsToJSON ctxPtr key'
          liftIO $ jsFreeValue ctxPtr key'
          return res

        case key of 
          String k -> do
            val <-  do
              val' <- liftIO $ C.withPtr_ $ \ptr ->
                [C.block| void { *$(JSValue *ptr) = JS_GetProperty($(JSContext *ctxPtr), *$(JSValueConst *objPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
              checkIsExpcetion ctxPtr val'
              res <- jsToJSON ctxPtr val'
              liftIO $ jsFreeValue ctxPtr val'
              return res

            xs <- collectVals properties objPtr (index+1) end
            return $ insert k val xs
          x -> throwM $ InternalError $ "Could not get property name" ++ show x

      | otherwise = return empty

    cleanup :: MonadIO m => Ptr (Ptr JSPropertyEnum) -> Int -> m ()
    cleanup properties plen = liftIO $ do
      forLoop plen $ \index -> do
        let i = fromIntegral index
        [C.block| void { JS_FreeAtom($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); }|]

      let void_ptr = castPtr properties
      [C.block| void { js_free($(JSContext *ctxPtr), *$(void **void_ptr)); }|]

      free properties



getErrorMessage :: MonadIO m => JSContextPtr -> m String
getErrorMessage ctxPtr = liftIO $ do
  ex <- C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_GetException($(JSContext *ctxPtr)); } |]
  res <- jsToString ctxPtr ex
  jsFreeValue ctxPtr ex
  return res



jsGetPropertyStr :: MonadIO m => JSContextPtr -> JSValue -> String -> m JSValue
jsGetPropertyStr ctxPtr val str = liftIO $
  C.withPtr_ $ \ptr -> withCString str $ \prop -> with val $ \valPtr ->
    [C.block| void { *$(JSValue *ptr) = JS_GetPropertyStr($(JSContext *ctxPtr), *$(JSValueConst *valPtr), $(const char *prop)); } |]


jsGetOwnPropertyNames :: (MonadThrow m, MonadIO m) => JSContextPtr -> JSValue -> Ptr (Ptr JSPropertyEnum) -> CInt -> m Int
jsGetOwnPropertyNames ctxPtr val properties flags = do
  (len,code) <- liftIO $ C.withPtr $ \plen -> with val $ \valPtr -> 
    [C.block| int { return JS_GetOwnPropertyNames($(JSContext *ctxPtr), $(JSPropertyEnum **properties), $(uint32_t *plen), *$(JSValueConst *valPtr), $(int flags)); } |]
  if code == 0 then return (fromIntegral len)
  else throwM $ InternalError "Could not get object properties"


jsCall :: JSContextPtr -> JSValue -> CInt -> (Ptr JSValue) -> IO JSValue
jsCall ctxt fun_obj argc argv = C.withPtr_ $ \res -> with fun_obj $ \funPtr -> 
  [C.block| void { *$(JSValue *res) = JS_Call($(JSContext *ctxt), *$(JSValueConst *funPtr), JS_NULL, $(int argc), $(JSValueConst *argv)); } |]


jsEval :: JSContextPtr -> CString -> CSize -> CString -> CInt -> IO JSValue
jsEval ctxPtr input input_len filename eval_flags = C.withPtr_ $ \ptr -> 
  [C.block| void { *$(JSValue *ptr) = JS_Eval($(JSContext *ctxPtr), $(const char *input), $(size_t input_len), $(const char *filename), $(int eval_flags)); } |]


evalRaw :: JSContextPtr -> JSEvalType -> String -> IO JSValue
evalRaw ctx eTyp code = 
    withCString "script.js" $ \cfilename ->
        withCStringLen code $ \(ccode, ccode_len) -> 
            jsEval ctx ccode (fromIntegral ccode_len) cfilename (toCType eTyp)




eval :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSEvalType -> String -> m JSValue
eval eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  checkIsExpcetion ctx val
  return val


toJSValue :: Aeson.ToJSON a => (MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => a -> m JSValue
toJSValue v = do
  ctx <- ask
  val <- jsonToJSValue ctx (Aeson.toJSON v)
  checkIsExpcetion ctx val `catch` (\(e:: JSRuntimeException) -> do {freeJSValue val ; throwM e})
  return val


fromJSValue_ :: (MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m Value
fromJSValue_ val = do
  ctx <- ask
  jsToJSON ctx val



fromJSValue :: (Aeson.FromJSON a, MonadCatch m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m a
fromJSValue val = do
  jsonval <- fromJSValue_ val

  case Aeson.fromJSON jsonval of
    Aeson.Success a -> return a
    Aeson.Error err -> throwM $ InternalError err



withJSValue :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m, Aeson.ToJSON a) => a -> (JSValue -> m b) -> m b
withJSValue v f = do
  ctx <- ask
  val <- jsonToJSValue ctx (Aeson.toJSON v)
  do { checkIsExpcetion ctx val ; f val } `finally` freeJSValue val




callRaw :: (MonadThrow m, MonadIO m) => JSContextPtr -> String -> [JSValue] -> m JSValue
callRaw ctxPtr funName args = do
    globalObject <- liftIO $ C.withPtr_ $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *globalObjectPtr) = JS_GetGlobalObject($(JSContext *ctxPtr)); } |]

    fun <- liftIO $ C.withPtr_ $ \funPtr -> withCString funName $ \cfunName -> with globalObject $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *funPtr) = JS_GetPropertyStr($(JSContext *ctxPtr), *$(JSValueConst *globalObjectPtr), $(const char *cfunName)); } |]

    liftIO $ jsFreeValue ctxPtr globalObject

    ty <- jsIs ctxPtr fun
    case ty of
      JSTypeFromTag JSTagException -> do
        err <- getErrorMessage ctxPtr 
        liftIO $ jsFreeValue ctxPtr fun
        throwM $ JSException err
      JSTypeFromTag JSTagUndefined -> throwM $ JSValueUndefined funName
      JSTypeFromTag JSTagObject -> do
        res <- liftIO $ withArrayLen args $ \len argv -> jsCall ctxPtr fun (fromIntegral $ len) argv
        liftIO $ jsFreeValue ctxPtr fun
        return res
      _ -> throwM $ JSValueIncorrectType {name = funName, expected = JSTypeFromTag JSTagObject, found = ty }


call :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => String -> [JSValue] -> m JSValue
call funName args = do
  ctx <- ask
  val <- callRaw ctx funName args
  checkIsExpcetion ctx val
  return val



call_ :: (MonadMask m, MonadReader JSContextPtr m, MonadIO m) => String -> [JSValue] -> m Value
call_ funName args = do
  ctx <- ask
  val <- callRaw ctx funName args
  do { checkIsExpcetion ctx val; jsToJSON ctx val } `finally` freeJSValue val


freeJSValue :: (MonadThrow m, MonadReader JSContextPtr m, MonadIO m) => JSValue -> m ()
freeJSValue val = do
  ctx <- ask
  liftIO $ jsFreeValue ctx val

-- quickjs :: (MonadIO m, MonadThrow m) => m b -> m b
quickjs :: MonadIO m => ReaderT (Ptr JSContext) m b -> m b
quickjs f = do
  rt <- liftIO $ jsNewRuntime
  ctx <- liftIO $ jsNewContext rt

  liftIO $ [C.block| void { 
    js_std_add_helpers($(JSContext *ctx), -1, NULL);
  } |]

  res <- (runReaderT f ctx) --`catchError` (\e -> do { cleanup ctx rt ; throwError e })
  -- we dont do cleanup, because there appears to be a leak:
  -- Assertion failed: (list_empty(&rt->gc_obj_list)), function JS_FreeRuntime, file quickjs/quickjs.c, line 1963.
  -- cleanup ctx rt
  return res
  -- where
    -- cleanup ctx rt = liftIO $ do
      -- jsFreeContext ctx
      -- jsFreeRuntime rt

-- -- quickjsIO :: IO a -> IO a
-- quickjsIO f = quickjs f `catch` \(err::JSRuntimeException) -> do
--   putStrLn "Quickjs error:"
--   putStrLn $ show err
