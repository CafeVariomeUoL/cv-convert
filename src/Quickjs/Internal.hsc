{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Quickjs.Internal where

import qualified Data.Map as Map
import           Foreign.C.Types
import           Foreign.Ptr (plusPtr)
import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

import           Language.C.Inline
import           Language.C.Inline.Context
import qualified Language.C.Types as C

#include "quickjs.h"

js_tag_first, js_tag_big_decimal, js_tag_big_int, js_tag_big_float, js_tag_symbol, js_tag_string, js_tag_module, js_tag_function_bytecode, js_tag_object,js_tag_int  ,js_tag_bool ,js_tag_null ,js_tag_undefined ,js_tag_uninitialized,js_tag_catch_offset,js_tag_exception ,js_tag_float64 :: CInt

js_tag_first       = -11
js_tag_big_decimal = -11
js_tag_big_int     = -10
js_tag_big_float   = -9
js_tag_symbol      = -8
js_tag_string      = -7
js_tag_module      = -3
js_tag_function_bytecode = -2
js_tag_object      = -1

js_tag_int         = 0
js_tag_bool        = 1
js_tag_null        = 2
js_tag_undefined   = 3
js_tag_uninitialized = 4
js_tag_catch_offset = 5
js_tag_exception   = 6
js_tag_float64     = 7


js_eval_type_global :: CUInt
js_eval_type_global = 0


js_gpn_string_mask, js_gpn_symbol_mask, js_gpn_private_mask, js_gpn_enum_only, js_gpn_set_enum :: CUInt
js_gpn_string_mask = 1
js_gpn_symbol_mask = 2
js_gpn_private_mask = 4
js_gpn_enum_only = 16
js_gpn_set_enum = 32

data JSValue = JSValue
  { u :: {-# UNPACK #-} !CDouble
  , tag :: {-# UNPACK #-} !CLong
  } deriving (Show, Eq)

instance Storable JSValue where
  sizeOf _ = #{size JSValue}
  alignment _ = #{alignment JSValue}
  peek ptr = do
--     stringName <- peekCString =<< peek (#{ptr Person, name} ptr)
    u <- peek (#{ptr JSValue, u} ptr)
    tag <- peek (#{ptr JSValue, tag} ptr)
    Prelude.pure (JSValue u tag)
  poke ptr (JSValue u tag) = do
--     cstring <- newCString name
--     poke (#{ptr Person, name} ptr) cstring
    poke (#{ptr JSValue, u} ptr) u
    poke (#{ptr JSValue, tag} ptr) tag

type JSValueConst = JSValue

newtype JSRuntime = JSRuntime { _unusedRuntime :: CUChar }

newtype JSContext = JSContext { _unusedContext :: CUChar }


type JSBool = CInt

type JSAtom = CUInt


data JSPropertyEnum = JSPropertyEnum
  { is_enumerable :: {-# UNPACK #-} !JSBool
  , atom :: {-# UNPACK #-} !JSAtom
  } deriving (Show, Eq)

instance Storable JSPropertyEnum where
  sizeOf _ = #{size JSPropertyEnum}
  alignment _ = #{alignment JSPropertyEnum}
  peek ptr = do
    is_enumerable <- peek (#{ptr JSPropertyEnum, is_enumerable} ptr)
    atom <- peek (#{ptr JSPropertyEnum, atom} ptr)
    Prelude.pure (JSPropertyEnum is_enumerable atom)
  poke ptr (JSPropertyEnum is_enumerable atom) = do
    poke (#{ptr JSPropertyEnum, is_enumerable} ptr) is_enumerable
    poke (#{ptr JSPropertyEnum, atom} ptr) atom



data JSRefCountHeader = JSRefCountHeader
  { ref_count :: {-# UNPACK #-} !CInt
  } deriving (Show, Eq)

instance Storable JSRefCountHeader where
  sizeOf _ = #{size JSRefCountHeader}
  alignment _ = #{alignment JSRefCountHeader}
  peek ptr = do
    ref_count <- peek (#{ptr JSRefCountHeader, ref_count} ptr)
    Prelude.pure (JSRefCountHeader ref_count)
  poke ptr (JSRefCountHeader ref_count) = do
    poke (#{ptr JSRefCountHeader, ref_count} ptr) ref_count




quickjsCtx :: Context
quickjsCtx = baseCtx <> fptrCtx <> ctx
  where
    ctx = mempty
      { ctxTypesTable = quickjsTypesTable
      }

quickjsTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
quickjsTypesTable = Map.fromList
  [ 
    (C.TypeName "JSValue", [t| JSValue |])
  , (C.TypeName "JSValueConst", [t| JSValueConst |])
  , (C.TypeName "JSRuntime", [t| JSRuntime |])
  , (C.TypeName "JSContext", [t| JSContext |])
  , (C.TypeName "JSBool", [t| JSBool |])
  , (C.TypeName "JSAtom", [t| JSAtom |])
  , (C.TypeName "JSPropertyEnum", [t| JSPropertyEnum |])
  ]

