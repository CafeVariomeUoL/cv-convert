{-# LANGUAGE BangPatterns, RecordWildCards, DuplicateRecordFields, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Quickjs (Raw.JSValue, JSContextPtr, EvalType(..), quickjsIO, call, eval, toJSValue, fromJSValue, fromJSValue_, freeJSValue) where

import Foreign
import Foreign.C
-- import Foreign.ForeignPtr.Unsafe
import qualified Language.C.Inline as C
import Control.Monad.Except(MonadError, throwError, runExceptT)

import Control.Monad.IO.Class(MonadIO, liftIO)

import Data.Aeson(Value(..))
import qualified Data.Aeson as Aeson

import Data.Scientific(fromFloatDigits, toRealFloat, toBoundedInteger, isInteger)
import Data.Text(Text, pack, unpack)
import Data.Vector(fromList, imapM_)
import Data.HashMap.Strict(HashMap, empty, insert, toList)
import Control.Monad.Except(catchError)
import Control.Monad.Trans.Except(ExceptT)
import Control.Monad(when, forM_)
import Control.Monad.Reader(MonadReader, runReaderT, ask)
import Control.Monad.Trans.Reader(ReaderT)
import Data.String.Conv(toS)

import qualified Quickjs.Internal as Raw



C.context Raw.quickjsCtx
C.include "quickjs.h"
C.include "quickjs-libc.h"




foreign import ccall "JS_NewRuntime"
  jsNewRuntime :: IO (Ptr Raw.JSRuntime)


foreign import ccall "JS_FreeRuntime"
  jsFreeRuntime :: Ptr Raw.JSRuntime -> IO ()



-- -- foreign import ccall "&JS_FreeRuntime"
-- --   jsRuntimeFinalizer :: FinalizerPtr JSRuntime
-- jsRuntimeFinalizer :: FinalizerPtr Raw.JSRuntime
-- jsRuntimeFinalizer = [C.funPtr| void free(JSRuntime *rt){ JS_FreeRuntime(rt); } |]




foreign import ccall "JS_NewContext"
  jsNewContext :: Ptr Raw.JSRuntime -> IO (Ptr Raw.JSContext)


foreign import ccall "JS_FreeContext"
  jsFreeContext :: Ptr Raw.JSContext -> IO ()



-- foreign import ccall "&JS_FreeContext"
--   jsContextFinalizer :: FinalizerPtr JSContext


-- jsContextFinalizer :: FinalizerPtr JSContext
-- jsContextFinalizer = [C.funPtr| void free(JSContext *ctx){ fprintf(stderr, "Releasing ctx\n"); JS_FreeContext(ctx); } |]




-- foreign import ccall "JS_AddIntrinsicOperators"
--   jsAddIntrinsicOperators :: Ptr Raw.JSContext -> IO ()


-- data JSContext = JSContext { rt :: JSRuntime, ctx :: ForeignPtr Raw.JSContext }

-- newContext :: MonadIO m => JSRuntime -> m JSContext
-- newContext rt = liftIO $ withForeignPtr rt $ \rtmPtr -> do
--     ptr <- jsNewContext rtmPtr
--     jsAddIntrinsicOperators ptr
--     ctx <- newForeignPtr jsContextFinalizer ptr
--     return Context{..}


-- jsValueFinalizer :: FinalizerEnvPtr Raw.JSContext Raw.JSValue
-- jsValueFinalizer = [C.funPtr| void free(JSContext *ctx, JSValue *v){
--     if (JS_VALUE_HAS_REF_COUNT(*v)) {
--         JSRefCountHeader *p = (JSRefCountHeader *)JS_VALUE_GET_PTR(*v);
--         if (--p->ref_count <= 0) {
--             __JS_FreeValue(ctx, *v);
--         }
--     }
--   } |]

jsFreeValue :: JSContextPtr -> Raw.JSValue -> IO ()
jsFreeValue ctx val = with val $ \v -> [C.block| void {
    if (JS_VALUE_HAS_REF_COUNT(*$(JSValue *v))) {
        JSRefCountHeader *p = (JSRefCountHeader *)JS_VALUE_GET_PTR(*$(JSValue *v));
        if (--p->ref_count <= 0) {
            __JS_FreeValue($(JSContext *ctx), *$(JSValue *v));
        }
    }
  } |]

type JSContextPtr = Ptr Raw.JSContext

-- type JSValuePtr = Ptr Raw.JSValue

type JSValueConstPtr = Ptr Raw.JSValueConst



-- newValuePtr :: JSContextPtr -> Raw.JSValue -> IO JSValueForeignPtr
-- newValuePtr ctxPtr val = do
--     jsval <- mallocForeignPtr
--     addForeignPtrFinalizerEnv jsValueFinalizer ctxPtr jsval
--     withForeignPtr jsval $ \ptr -> poke ptr val
--     return jsval



jsIsException :: MonadIO m => Raw.JSValue -> m Bool
jsIsException val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsException(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsNull :: MonadIO m => Raw.JSValue -> m Bool
jsIsNull  val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsNull(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsUndefined :: MonadIO m => Raw.JSValue -> m Bool
jsIsUndefined  val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsUndefined(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsBool :: MonadIO m => Raw.JSValue -> m Bool
jsIsBool val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsBool(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsNumber :: MonadIO m => Raw.JSValue -> m Bool
jsIsNumber val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsNumber(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsString :: MonadIO m => Raw.JSValue -> m Bool
jsIsString val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsString(*$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsArray :: MonadIO m => JSContextPtr -> Raw.JSValue -> m Bool
jsIsArray ctxPtr val = do
  b <- liftIO $ with val $ \valPtr -> 
    [C.block| int { return JS_IsArray($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
  return $ b == 1

jsIsObject :: MonadIO m => Raw.JSValue -> m Bool
jsIsObject val = do
  b <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_IsObject(*$(JSValueConst *valPtr)); } |]
  return $ b == 1






jsNullValue :: Raw.JSValue
jsNullValue = Raw.JSValue { u = 0, tag = fromIntegral Raw.js_tag_null }


jsNewBool :: JSContextPtr -> Bool -> IO Raw.JSValue
jsNewBool ctxPtr bool = do
  let b = if bool then 1 else 0
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewBool($(JSContext *ctxPtr), $(int b)); } |]

jsNewFloat64 :: JSContextPtr -> CDouble -> IO Raw.JSValue
jsNewFloat64 ctxPtr num =
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewFloat64($(JSContext *ctxPtr), $(double num)); } |]

jsNewInt64 :: JSContextPtr -> Int64 -> IO Raw.JSValue
jsNewInt64 ctxPtr num = do
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_NewInt64($(JSContext *ctxPtr), $(int64_t num)); } |]

jsNewString :: JSContextPtr -> String -> IO Raw.JSValue
jsNewString ctxPtr s = C.withPtr_ $ \ptr -> withCStringLen s $ \(cstringPtr, cstringLen) -> do
  let len = fromIntegral cstringLen
  [C.block| void { *$(JSValue *ptr) = JS_NewStringLen($(JSContext *ctxPtr), $(const char *cstringPtr), $(size_t len)); } |]



-- withJSValue :: (MonadError String m, MonadIO m) => JSContext -> JSValue -> (Ptr JSValue -> m b) -> m b
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



checkIsExpcetion :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m ()
checkIsExpcetion ctxPtr val = do
  isExn <- jsIsException val
  when isExn $ do
    err <- getErrorMessage ctxPtr 
    liftIO $ jsFreeValue ctxPtr val
    throwError err



jsonToJSValue :: (MonadError String m, MonadIO m) => JSContextPtr -> Value -> m Raw.JSValue
jsonToJSValue _ Null = pure jsNullValue
jsonToJSValue ctx (Bool b) = liftIO $ jsNewBool ctx b
jsonToJSValue ctx (Number n) = 
  if not (isInteger n) then liftIO $ jsNewFloat64 ctx (toRealFloat n)
  else case toBoundedInteger n of
    Just i -> liftIO $ jsNewInt64 ctx i
    Nothing -> throwError "Value does not fit in Int64"
jsonToJSValue ctx (String s) = liftIO $ jsNewString ctx (unpack s) 
jsonToJSValue ctxPtr (Array xs) = do
  arrVal <- liftIO (C.withPtr_ $ \arrValPtr -> [C.block| void { *$(JSValueConst *arrValPtr) = JS_NewArray($(JSContext *ctxPtr)); } |])
  
  checkIsExpcetion ctxPtr arrVal

  flip imapM_ xs $ \index value -> do 
    val <- jsonToJSValue ctxPtr value
    isExn <- jsIsException val
    when isExn $ do
      err <- getErrorMessage ctxPtr 
      liftIO $ jsFreeValue ctxPtr arrVal
      throwError err

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
      throwError "Could not append element to array"
    else return ()

  return arrVal
jsonToJSValue ctxPtr (Object o) = do
  objVal <- liftIO (C.withPtr_ $ \objValPtr -> 
    [C.block| void { *$(JSValueConst *objValPtr) = JS_NewObject($(JSContext *ctxPtr)); } |])

  checkIsExpcetion ctxPtr objVal
  
  forM_ (toList o) $ \(key,value) -> do
    val <- jsonToJSValue ctxPtr value
    isExn <- jsIsException val
    when isExn $ do
      err <- getErrorMessage ctxPtr 
      liftIO $ jsFreeValue ctxPtr objVal
      throwError err

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
      throwError "Could not add add property to object"

  return objVal


jsToBool :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m Bool
jsToBool ctxPtr val = do
    code <- liftIO $ with val $ \valPtr -> [C.block| int { return JS_ToBool($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    case code of
        -1 -> getErrorMessage ctxPtr >>= throwError
        0 -> return False
        _ -> return True

jsToInt64 :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m Int64
jsToInt64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \intPtr -> with val $ \valPtr -> [C.block| int { return JS_ToInt64($(JSContext *ctxPtr), $(int64_t *intPtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwError


jsToFloat64 :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m CDouble
jsToFloat64 ctxPtr val = do
  (res, code) <- liftIO $ C.withPtr $ \doublePtr -> with val $ \valPtr -> [C.block| int { return JS_ToFloat64($(JSContext *ctxPtr), $(double *doublePtr), *$(JSValueConst *valPtr)); } |]
  if code == 0 then return res
  else getErrorMessage ctxPtr >>= throwError



jsToString :: MonadIO m => JSContextPtr -> Raw.JSValue -> m String
jsToString ctxPtr val = liftIO $ do
    cstring <- with val $ \valPtr -> [C.block| const char * { return JS_ToCString($(JSContext *ctxPtr), *$(JSValueConst *valPtr)); } |]
    if cstring == nullPtr then return ""
    else do
      string <- peekCString cstring
      jsFreeCString ctxPtr cstring
      return string


foreign import ccall "JS_FreeCString"
  jsFreeCString :: JSContextPtr -> CString -> IO ()


jsToJSON :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m Value
jsToJSON ctx jsval = do
  isNull <- jsIsNull jsval
  isUndef <- jsIsUndefined jsval
  if isNull || isUndef then return Null
  else do
    isBool <- jsIsBool jsval
    if isBool then do
      b <- jsToBool ctx jsval
      return $ Bool b
    else do
      isNumber <- jsIsNumber jsval
      if isNumber then do
        n <- jsToFloat64 ctx jsval
        return $ Number $ fromFloatDigits n
      else do
        isString <- jsIsString jsval
        if isString then do
          s <- jsToString ctx jsval
          return $ String $ pack s
        else do
          isArray <- jsIsArray ctx jsval
          if isArray then do
            len <- do
              lenVal <- jsGetPropertyStr ctx jsval "length" 
              len' <- jsToInt64 ctx lenVal
              liftIO $ jsFreeValue ctx lenVal
              return len'
            vs <- jsArrayToJSON ctx jsval 0 (fromIntegral len)
            return $ Array $ fromList vs
          else do
            isObject <- jsIsObject jsval
            if isObject then do
              o <- jsObjectToJSON ctx jsval
              return $ Object o 
            else do
              throwError $ "Unsupported type tag: " ++ (show $ Raw.tag jsval)

jsArrayToJSON :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> Int -> Int -> m [Value]
jsArrayToJSON ctxPtr jsval index len = 
  if index < len then do
    v <- do
      let idx = fromIntegral index
      val <- liftIO $ C.withPtr_ $ \ptr -> with jsval $ \jsvalPtr -> 
        [C.block| void { *$(JSValue *ptr) = JS_GetPropertyUint32($(JSContext *ctxPtr), *$(JSValueConst *jsvalPtr), $(uint32_t idx)); } |]
      isExn <- jsIsException val
      if isExn then getErrorMessage ctxPtr >>= throwError
      else do
        res <- jsToJSON ctxPtr val
        liftIO $ jsFreeValue ctxPtr val
        return res

    vs <- jsArrayToJSON ctxPtr jsval (index+1) len
    return $ v:vs
  else return []



forLoop :: (Num a, Ord a, Monad m) => a -> (a -> m ()) -> m ()
forLoop end f = go 0
  where
    go !x | x < end  = f x >> go (x+1)
          | otherwise = return ()



jsObjectToJSON :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> m (HashMap Text Value)
jsObjectToJSON ctxPtr obj = do
    let flags = Raw.js_gpn_string_mask .|. Raw.js_gpn_symbol_mask .|. Raw.js_gpn_enum_only
    properties <- liftIO $ malloc
    plen <- jsGetOwnPropertyNames ctxPtr obj properties (fromIntegral flags) 
      `catchError` (\e -> do
        liftIO $ free properties
        throwError e
      )
    objPtr <- liftIO $ malloc
    liftIO $ poke objPtr obj

    res <- collectVals properties objPtr 0 plen `catchError` (\e -> do
        liftIO $ free objPtr
        throwError e
      )
    cleanup properties plen
    return res
  where

    collectVals :: (MonadError String m, MonadIO m) => Ptr (Ptr Raw.JSPropertyEnum) -> JSValueConstPtr -> Int -> Int -> m (HashMap Text Value)
    collectVals properties objPtr !index end 
      | index < end = do
        let i = fromIntegral index

        key <- do
          key' <- liftIO $ C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_AtomToString($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
          isExn <- liftIO $ jsIsException key'
          if isExn then getErrorMessage ctxPtr >>= throwError
          else do
            res <- jsToJSON ctxPtr key'
            liftIO $ jsFreeValue ctxPtr key'
            return res

        case key of 
          String k -> do
            val <-  do
              val' <- liftIO $ C.withPtr_ $ \ptr ->
                [C.block| void { *$(JSValue *ptr) = JS_GetProperty($(JSContext *ctxPtr), *$(JSValueConst *objPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); } |]
              isExn <- liftIO $ jsIsException val'
              if isExn then getErrorMessage ctxPtr >>= throwError
              else do
                res <- jsToJSON ctxPtr val'
                liftIO $ jsFreeValue ctxPtr val'
                return res

            xs <- collectVals properties objPtr (index+1) end
            return $ insert k val xs
          x -> throwError $ "Could not get property name" ++ show x

      | otherwise = return empty




    cleanup :: (MonadError String m, MonadIO m) => Ptr (Ptr Raw.JSPropertyEnum) -> Int -> m ()
    cleanup properties plen = liftIO $ do
      forLoop plen $ \index -> do
        let i = fromIntegral index
        [C.block| void { JS_FreeAtom($(JSContext *ctxPtr), (*$(JSPropertyEnum **properties))[$(uint32_t i)].atom); }|]

      let void_ptr = castPtr properties
      [C.block| void { js_free($(JSContext *ctxPtr), *$(void **void_ptr)); }|]

      free properties


-- jsGetException :: JSContextPtr -> IO JSValuePtr
-- jsGetException ctxPtr = do
--   ptr <- malloc
--   [C.block| void { *$(JSValue *ptr) = JS_GetException($(JSContext *ctxPtr)); } |]
--   return ptr


getErrorMessage :: MonadIO m => JSContextPtr -> m String
getErrorMessage ctxPtr = liftIO $ do
  ex <- C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_GetException($(JSContext *ctxPtr)); } |]
  res <- jsToString ctxPtr ex
  jsFreeValue ctxPtr ex
  return res



jsGetPropertyStr :: MonadIO m => JSContextPtr -> Raw.JSValue -> String -> m Raw.JSValue
jsGetPropertyStr ctxPtr val str = liftIO $
  C.withPtr_ $ \ptr -> withCString str $ \prop -> with val $ \valPtr ->
    [C.block| void { *$(JSValue *ptr) = JS_GetPropertyStr($(JSContext *ctxPtr), *$(JSValueConst *valPtr), $(const char *prop)); } |]


-- jsGetPropertyInt :: MonadIO m => JSContextPtr -> JSValuePtr -> Int -> m JSValuePtr
-- jsGetPropertyInt ctxPtr valPtr i = liftIO $ do
--   ptr <- malloc
--   let idx = fromIntegral i
--   [C.block| void { *$(JSValue *ptr) = JS_GetPropertyUint32($(JSContext *ctxPtr), *$(JSValueConst *valPtr), $(uint32_t idx)); } |]
--   return ptr


jsGetOwnPropertyNames :: (MonadError String m, MonadIO m) => JSContextPtr -> Raw.JSValue -> Ptr (Ptr Raw.JSPropertyEnum) -> CInt -> m Int
jsGetOwnPropertyNames ctxPtr val properties flags = do
  (len,code) <- liftIO $ C.withPtr $ \plen -> with val $ \valPtr -> 
    [C.block| int { return JS_GetOwnPropertyNames($(JSContext *ctxPtr), $(JSPropertyEnum **properties), $(uint32_t *plen), *$(JSValueConst *valPtr), $(int flags)); } |]
  if code == 0 then return (fromIntegral len)
  else throwError "Could not get object properties"


jsCall :: JSContextPtr -> Raw.JSValue -> CInt -> (Ptr Raw.JSValue) -> IO Raw.JSValue
jsCall ctxt fun_obj argc argv = C.withPtr_ $ \res -> with fun_obj $ \funPtr -> 
  [C.block| void { *$(JSValue *res) = JS_Call($(JSContext *ctxt), *$(JSValueConst *funPtr), JS_NULL, $(int argc), $(JSValueConst *argv)); } |]


jsEval :: JSContextPtr -> CString -> CSize -> CString -> CInt -> IO Raw.JSValue
jsEval ctxPtr input input_len filename eval_flags =
  C.withPtr_ $ \ptr -> [C.block| void { *$(JSValue *ptr) = JS_Eval($(JSContext *ctxPtr), $(const char *input), $(size_t input_len), $(const char *filename), $(int eval_flags)); } |]


evalRaw :: JSContextPtr -> EvalType -> String -> IO Raw.JSValue
evalRaw ctx eTyp code = 
    withCString "script.js" $ \cfilename ->
        withCStringLen code $ \(ccode, ccode_len) -> 
            jsEval ctx ccode (fromIntegral ccode_len) cfilename (fromIntegral $ case eTyp of 
                Global -> Raw.js_eval_type_global
                Module -> Raw.js_eval_type_module
              )


-- eval_ :: (MonadError String m, MonadIO m) => Context -> String -> m ()
-- eval_ ctx code = withJSValuePtr ctx $ \ptr -> do
--     liftIO $ evalRaw ctx code ptr
--     isExn <- liftIO $ jsIsException ptr
--     when isExn $ getErrorMessage ctx >>= throwError


-- evalWithValue :: (MonadError String m, MonadIO m) => Context -> String -> (Ptr JSValue -> m b) -> m b
-- evalWithValue ctx code f = withJSValuePtr ctx $ \ptr -> do
--     liftIO $ evalRaw ctx code ptr
--     isExn <- liftIO $ jsIsException ptr
--     when isExn $ getErrorMessage ctx >>= throwError
--     f ptr


data EvalType = Global | Module

eval :: (MonadError String m, MonadReader JSContextPtr m, MonadIO m) => EvalType -> String -> m Raw.JSValue
eval eTyp code = do
  ctx <- ask
  val <- liftIO $ evalRaw ctx eTyp code
  isExn <- liftIO $ jsIsException val
  when isExn $ do
    e <- getErrorMessage ctx
    liftIO $ jsFreeValue ctx val
    throwError e
  return val


toJSValue :: Aeson.ToJSON a => (MonadError String m, MonadReader JSContextPtr m, MonadIO m) => a -> m Raw.JSValue
toJSValue v = do
  ctx <- ask
  val <- jsonToJSValue ctx (Aeson.toJSON v)
  isExn <- liftIO $ jsIsException val
  when isExn $ do
    e <- getErrorMessage ctx
    liftIO $ jsFreeValue ctx val
    throwError e
  return val


fromJSValue_ :: (MonadError String m, MonadReader JSContextPtr m, MonadIO m) => Raw.JSValue -> m Value
fromJSValue_ val = do
  ctx <- ask
  jsToJSON ctx val


fromJSValue :: (Aeson.FromJSON a, MonadError String m, MonadReader JSContextPtr m, MonadIO m) => Raw.JSValue -> m a
fromJSValue val = do
  jsonval <- fromJSValue_ val

  case Aeson.fromJSON jsonval of
    Aeson.Success a -> return a
    Aeson.Error str -> throwError str

-- eval_ :: (MonadError String m, MonadReader Context m, MonadIO m) => String -> m ()
-- eval_ code = do
--   ctx <- ask
--   evalWithValue ctx code (\_ -> pure ())



callRaw :: (MonadError String m, MonadIO m) => JSContextPtr -> String -> [Raw.JSValue] -> m Raw.JSValue
callRaw ctxPtr funName args = do
    globalObject <- liftIO $ C.withPtr_ $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *globalObjectPtr) = JS_GetGlobalObject($(JSContext *ctxPtr)); } |]

    fun <- liftIO $ C.withPtr_ $ \funPtr -> withCString funName $ \cfunName -> with globalObject $ \globalObjectPtr ->
      [C.block| void { *$(JSValue *funPtr) = JS_GetPropertyStr($(JSContext *ctxPtr), *$(JSValueConst *globalObjectPtr), $(const char *cfunName)); } |]

    liftIO $ jsFreeValue ctxPtr globalObject

    isExn <- liftIO $ jsIsException fun
    when isExn $ do
      liftIO $ jsFreeValue ctxPtr fun
      throwError $ "Exception while getting " ++ funName
    isUndefined <- liftIO $ jsIsUndefined fun
    when isUndefined $ do
      liftIO $ jsFreeValue ctxPtr fun
      throwError $ "Function '" ++ funName ++ "' not found"
    isObj <- liftIO $ jsIsObject fun
    when (not isObj) $ do
      liftIO $ jsFreeValue ctxPtr fun
      throwError $ funName ++ " is not an object"
    res <- liftIO $ withArrayLen args $ \len argv -> jsCall ctxPtr fun (fromIntegral $ len) argv
    liftIO $ jsFreeValue ctxPtr fun
    return res





call :: (MonadError String m, MonadReader JSContextPtr m, MonadIO m) => String -> [Raw.JSValue] -> m Raw.JSValue
call funName args = do
  ctx <- ask
  val <- callRaw ctx funName args

  isExn <- liftIO $ jsIsException val
  when isExn $ do
    e <- getErrorMessage ctx
    liftIO $ jsFreeValue ctx val
    throwError e

  return val


freeJSValue :: (MonadError String m, MonadReader JSContextPtr m, MonadIO m) => Raw.JSValue -> m ()
freeJSValue val = do
  ctx <- ask
  liftIO $ jsFreeValue ctx val

quickjs :: MonadIO m =>
  ReaderT (Ptr Raw.JSContext) m b -> m b
quickjs f = do
    rt <- liftIO $ jsNewRuntime
    ctx <- liftIO $ jsNewContext rt

    liftIO $ [C.block| void { 
      JSValue global_obj, console;

      global_obj = JS_GetGlobalObject($(JSContext *ctx));
      console = JS_NewObject($(JSContext *ctx));

      JS_SetPropertyStr($(JSContext *ctx), console, "log",
                        JS_NewCFunction($(JSContext *ctx), js_print, "log", 1));
      JS_SetPropertyStr($(JSContext *ctx), global_obj, "console", console);

      JS_FreeValue($(JSContext *ctx), global_obj);
    } |]

    res <- (runReaderT f ctx) --`catchError` (\e -> do { cleanup ctx rt ; throwError e })
    -- cleanup ctx rt
    return res
  -- where
  --   cleanup ctx rt = liftIO $ do
  --     performGC
  --     -- jsFreeContext ctx
  --     -- jsFreeRuntime rt

quickjsIO :: ReaderT (Ptr Raw.JSContext) (ExceptT String IO) a -> IO ()
quickjsIO f = do
  res <- runExceptT $ quickjs f
  case res of
    Left err -> do
      putStrLn "Quickjs error:"
      putStrLn err
    Right _ -> pure ()
