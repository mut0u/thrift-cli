{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Cmd.Lib ( sendFunc
               , buildRequest
               ) where

import qualified Data.IORef as R
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict((!))
import qualified Data.Int as I
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V
import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Language.Thrift.Parser as LT
import qualified Language.Thrift.AST as LT
import qualified Data.Maybe as M (catMaybes, fromJust)
import qualified Data.Scientific as S
import Data.List (find)
import System.IO
import qualified Control.Monad as M ( liftM, ap, when )
import qualified Control.Exception as X
import Data.List.Split (splitOn)
import qualified Text.Megaparsec.Pos
import Debug.Trace

import Thrift.Protocol.Binary (Protocol)
import Thrift.Transport.Handle (Transport)


import System.FilePath.Posix (takeDirectory, takeBaseName, FilePath)


seqid = R.newIORef 0

-- remove field name prefix like a.b to b
removePrefix :: Text -> Text
removePrefix name =  if length (splitOn "." $ unpack name) == 2
                     then let [_, name'] = (splitOn "." $ unpack name)
                          in  pack name'
                     else name



typeTransformer (LT.StringType _ _) = T.T_STRING
typeTransformer (LT.BoolType _ _ )  = T.T_BOOL
typeTransformer (LT.ByteType _ _) = T.T_BYTE
typeTransformer (LT.I16Type _ _) = T.T_I16
typeTransformer (LT.I32Type _ _) = T.T_I32
typeTransformer (LT.I64Type _ _) = T.T_I64
typeTransformer (LT.DoubleType _ _) = T.T_DOUBLE
typeTransformer _ = error "can not find type"


buildListType :: LT.TypeReference srcAnnot -> T.ThriftType
buildListType typeName =  T.T_LIST $  typeTransformer typeName

buildMapType :: LT.TypeReference srcAnnot -> LT.TypeReference srcAnnot -> T.ThriftType
buildMapType keyTypeName valTypeName = T.T_MAP (typeTransformer keyTypeName) (typeTransformer valTypeName)


buildStringValue :: DA.Value -> T.ThriftVal
buildStringValue (DA.String s) = T.TString $ B.pack (unpack s)
buildStringValue v = error $ "string type error" ++ show v

buildBoolValue :: DA.Value -> T.ThriftVal
buildBoolValue (DA.Bool b) =  T.TBool b
buildBoolValue v = error $ "bool type error" ++ show v

buildByteValue :: DA.Value -> T.ThriftVal
buildByteValue (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TByte ((fromIntegral i) :: I.Int8)
                                       Left _ -> error $ "byte type error" ++ show b
buildByteValue v = error $ "byte type error" ++ show v


buildInt16Value :: DA.Value -> T.ThriftVal
buildInt16Value (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TI16 ((fromIntegral i) :: I.Int16)
                                       Left _ -> error $ "int16 type error" ++ show b
buildInt16Value v = error $ "int16 type error" ++ show v



buildInt32Value :: DA.Value -> T.ThriftVal
buildInt32Value (DA.Number b) = case (S.floatingOrInteger b) of
                                  Right i -> T.TI32 ((fromIntegral i) :: I.Int32)
                                  Left _ -> error $ "int32 type error" ++ show b
buildInt32Value v = error $ "int32 type error" ++ show v


buildInt64Value :: DA.Value -> T.ThriftVal
buildInt64Value (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TI64 ((fromIntegral i) :: I.Int64)
                                       Left _ -> error $ "int64 type error" ++ show b
buildInt64Value v = error $ "int64 type error" ++ show v


buildDoubleValue:: DA.Value -> T.ThriftVal
buildDoubleValue (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right _ ->  error $ "double type error" ++ show b
                                       Left d -> T.TDouble d
buildDoubleValue v = error $ "int64 type error" ++ show v


buildListValue
  :: LT.Program Text.Megaparsec.Pos.SourcePos
     -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
     -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
     -> DA.Value
     -> T.ThriftVal
buildListValue p hm typeName (DA.Array arr) =  T.TList (T.T_LIST $ typeTransformer typeName) $ map (\x -> buildValue p hm typeName x) $ V.toList arr
buildListValue p hm typeName v = error $ "list type error" ++ show v


buildMapValue
  :: LT.Program Text.Megaparsec.Pos.SourcePos
     -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
     -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
     -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
     -> DA.Value
     -> T.ThriftVal
buildMapValue p hm keyTypeName valTypeName (DA.Object m) =
  let kvThriftVals = map (\ (k,v) -> ( (buildValue p hm keyTypeName $ DA.String k)
                                     , (buildValue p hm valTypeName v))) $ Map.toList m
  in T.TMap (typeTransformer keyTypeName) (typeTransformer valTypeName) kvThriftVals

buildMapValue p hm keyTypeName valTypeName val = error $ "list type error" ++ show val



buildInnerStruct
  :: LT.Program Text.Megaparsec.Pos.SourcePos
     -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
     -> Text
     -> DA.Value
     -> T.ThriftVal
buildInnerStruct p hm typeName (DA.Object mapVal) = buildStruct p hm (structDecl p typeName) mapVal
buildInnerStruct p hm typeName val = error $ "struct type error" ++ show val


buildValue :: LT.Program Text.Megaparsec.Pos.SourcePos
           -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
           -> DA.Value
           -> T.ThriftVal
buildValue p hm (LT.StringType _ _) v = buildStringValue v
buildValue p hm (LT.BoolType _ _ ) v = buildBoolValue v
buildValue p hm ( LT.ByteType _ _) v =  buildByteValue v
buildValue p hm ( LT.I16Type _ _) v =  buildInt16Value v
buildValue p hm ( LT.I32Type _ _) v =  buildInt32Value v
buildValue p hm ( LT.I64Type _ _) v =  buildInt64Value v
buildValue p hm ( LT.DoubleType _ _) v =  buildDoubleValue v
buildValue p hm ( LT.ListType typeName _ _) v = buildListValue p hm typeName v
buildValue p hm ( LT.MapType keyTypeName valTypeName _ _) v = buildMapValue p hm keyTypeName valTypeName v
buildValue p hm ( LT.DefinedType typeName _) v = buildInnerStruct p hm (removePrefix typeName) v



buildField :: LT.Program Text.Megaparsec.Pos.SourcePos
           -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.Field Text.Megaparsec.Pos.SourcePos
           -> DA.Object -> Maybe (I.Int16, (L.Text, T.ThriftVal))
buildField p hm f o = case mfval of Nothing-> Nothing
                                    Just fval -> Just (idx, ( L.fromStrict fname
                                                            , buildValue p' hm ftype $ M.fromJust mfval))
  where fname = LT.fieldName f
        name = case LT.fieldValueType f of
                 LT.DefinedType sName _ -> sName
                 _ -> error "not impl type for "
        hm' = hm
        (p', name') = if length (splitOn "." $ unpack name) == 2
                      then let [prefix, name''] = (splitOn "." $ unpack name)
                           in  (hm ! prefix, name'')
                      else (p, unpack name)

        ftype = LT.fieldValueType f
        idx = fromIntegral (M.fromJust $ LT.fieldIdentifier f) :: I.Int16
        mfval = Map.lookup fname o


buildStruct :: LT.Program Text.Megaparsec.Pos.SourcePos
            -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
            -> LT.Struct Text.Megaparsec.Pos.SourcePos
            -> DA.Object
            -> T.ThriftVal
buildStruct p hm decl obj = T.TStruct $ Map.fromList $ M.catMaybes buildFields
  where fields = LT.structFields decl
        buildFields :: [Maybe (I.Int16, (L.Text, T.ThriftVal))]
        buildFields = map (\f -> buildField p hm f obj) fields


structDecl :: LT.Program Text.Megaparsec.Pos.SourcePos -> Text -> LT.Struct Text.Megaparsec.Pos.SourcePos
structDecl p name = M.fromJust $ find (\x -> LT.structName x == name) structList
  where
    structList = map ((\ (LT.StructType s) -> s) . (\ (LT.TypeDefinition x) -> x))
                 $ filter (\ (LT.TypeDefinition x) -> case x of
                               (LT.StructType _) -> True
                               -- 暂时只解析StructType 类型
                               _ -> False) $
                 filter (\ x ->
                   case x of
                     LT.TypeDefinition _ -> True
                     _ -> False)
         $ LT.programDefinitions p


buildRequestParam
  :: LT.Program Text.Megaparsec.Pos.SourcePos
     -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
     -> LT.Field Text.Megaparsec.Pos.SourcePos
     -> Map.HashMap Text DA.Value
     -> (I.Int16, (L.Text, T.ThriftVal))
buildRequestParam p hm field obj = let idx = fromIntegral (M.fromJust (LT.fieldIdentifier field))
                                       name = case LT.fieldValueType field of
                                                LT.DefinedType sName _ -> sName
                                                _ -> error "not impl type for "
                                       sd = (structDecl p name)
                                       tVal = buildStruct p hm sd (findObject (LT.fieldName field) obj)
                                   in (idx, (L.pack $ unpack name, tVal))
  where
    findObject :: Text -> DA.Object -> DA.Object
    findObject k obj = case v of
                         DA.Object o -> o
                         _ -> error "error"
      where
        v = M.fromJust (Map.lookup k obj)

buildRequest :: LT.Program Text.Megaparsec.Pos.SourcePos
             -> Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
             -> Text -> Text -> DA.Object -> T.ThriftVal
buildRequest p hm sName fName obj = T.TStruct $ Map.fromList fields
  where
    Just serviceDecl = find (\x -> LT.serviceName x == sName) $
      map (\ (LT.ServiceDefinition x) -> x) $
      filter (\ x -> case x of
                   LT.ServiceDefinition _ -> True
                   _ -> False) $
      LT.programDefinitions p

    Just funcDecl = find (\x -> LT.functionName x == fName) $ LT.serviceFunctions serviceDecl
    fields = map (\x-> buildRequestParam p hm x obj) $ LT.functionParameters funcDecl


buildTypeMap :: Text -> T.TypeMap
buildTypeMap name = undefined

sendFunc :: (Protocol p, Protocol p, Transport t, Transport t) =>
            (p t, p t) ->
            LT.Program Text.Megaparsec.Pos.SourcePos ->
            Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos) ->
            Text ->
            Text ->
            DA.Object ->
            IO (Map.HashMap I.Int16 (L.Text, T.ThriftVal))
sendFunc (ip,op) p hm sName fName vals = do
  seq <- seqid
  seqn <- R.readIORef seq
  let req = buildRequest p hm sName fName vals

  T.writeMessageBegin op (L.fromStrict fName, T.M_CALL, seqn)
  T.writeVal op req
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
  let resTypeName = ""
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }

  let resultType = Map.fromList [(0,("success",(T.T_STRUCT (buildTypeMap resTypeName))))]

  thriftVal <- T.readVal ip (T.T_STRUCT resultType)
  T.readMessageEnd ip
  let T.TStruct mapVal = thriftVal
  let Just (_, T.TStruct retVal)  = Map.lookup (0 :: I.Int16) mapVal
  return retVal
