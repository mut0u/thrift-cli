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
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
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
--buildStringValue (DA.String s) = T.TString $  B.pack (unpack s)
buildStringValue (DA.String s) = T.TString $ B.fromStrict $ encodeUtf8 s
buildStringValue v = error $ "string type error" ++ show v

mkJsonStringValue :: T.ThriftVal -> DA.Value
mkJsonStringValue (T.TString s) = DA.String $ decodeUtf8 . B.toStrict $ s
mkJsonStringValue v = error $ "string type error" ++ show v


buildBoolValue :: DA.Value -> T.ThriftVal
buildBoolValue (DA.Bool b) =  T.TBool b
buildBoolValue v = error $ "bool type error" ++ show v

mkJsonBoolValue :: T.ThriftVal -> DA.Value
mkJsonBoolValue (T.TBool b) =  DA.Bool b
mkJsonBoolValue v = error $ "bool type error" ++ show v


buildByteValue :: DA.Value -> T.ThriftVal
buildByteValue (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TByte ((fromIntegral i) :: I.Int8)
                                       Left _ -> error $ "byte type error" ++ show b
buildByteValue v = error $ "byte type error" ++ show v


mkJsonByteValue :: T.ThriftVal -> DA.Value
mkJsonByteValue (T.TByte b) = DA.Number (read (show b) :: S.Scientific)
mkJsonByteValue v = error $ "byte type error" ++ show v



buildInt16Value :: DA.Value -> T.ThriftVal
buildInt16Value (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TI16 ((fromIntegral i) :: I.Int16)
                                       Left _ -> error $ "int16 type error" ++ show b
buildInt16Value v = error $ "int16 type error" ++ show v


-- todo fix the integer parse to float.
mkJsonInt16Value :: T.ThriftVal -> DA.Value
mkJsonInt16Value (T.TI16 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt16Value v = error $ "int16 type error" ++ show v


buildInt32Value :: DA.Value -> T.ThriftVal
buildInt32Value (DA.Number b) = case (S.floatingOrInteger b) of
                                  Right i -> T.TI32 ((fromIntegral i) :: I.Int32)
                                  Left _ -> error $ "int32 type error" ++ show b
buildInt32Value v = error $ "int32 type error" ++ show v

mkJsonInt32Value :: T.ThriftVal -> DA.Value
mkJsonInt32Value (T.TI32 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt32Value v = error $ "int32 type error" ++ show v

buildInt64Value :: DA.Value -> T.ThriftVal
buildInt64Value (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right i -> T.TI64 ((fromIntegral i) :: I.Int64)
                                       Left _ -> error $ "int64 type error" ++ show b
buildInt64Value v = error $ "int64 type error" ++ show v

mkJsonInt64Value :: T.ThriftVal -> DA.Value
mkJsonInt64Value (T.TI64 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt64Value v = error $ "int64 type error" ++ show v

buildDoubleValue :: DA.Value -> T.ThriftVal
buildDoubleValue (DA.Number b) = case (S.floatingOrInteger b) of
                                       Right _ ->  error $ "double type error" ++ show b
                                       Left d -> T.TDouble d
buildDoubleValue v = error $ "int64 type error" ++ show v

mkJsonDoubleValue :: T.ThriftVal -> DA.Value
mkJsonDoubleValue (T.TDouble i) = DA.Number (read (show i) :: S.Scientific)
mkJsonDoubleValue v = error $ "double type error" ++ show v


buildListValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> DA.Value
               -> T.ThriftVal
buildListValue ps typeName (DA.Array arr) =  T.TList (T.T_LIST $ typeTransformer typeName) $ map (\x -> buildValue ps typeName x) $ V.toList arr
buildListValue ps typeName v = error $ "list type error" ++ show v


mkJsonArrayValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                 -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
                 -> T.ThriftVal
                 -> DA.Value
mkJsonArrayValue ps typeName (T.TList lType vals) =  DA.Array (V.fromList $ map (\v -> mkJsonValue ps typeName v) vals)
mkJsonArrayValue ps typeName v = error $ "list type error" ++ show v


buildMapValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
              -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
              -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
              -> DA.Value
              -> T.ThriftVal
buildMapValue ps keyTypeName valTypeName (DA.Object m) =
  let kvThriftVals = map (\ (k,v) -> ( (buildValue ps keyTypeName $ DA.String k)
                                     , (buildValue ps valTypeName v))) $ Map.toList m
  in T.TMap (typeTransformer keyTypeName) (typeTransformer valTypeName) kvThriftVals

buildMapValue ps keyTypeName valTypeName val = error $ "list type error" ++ show val


mkJsonMapValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> T.ThriftVal
               -> DA.Value
mkJsonMapValue ps (LT.StringType _ _) valTypeName (T.TMap kType vType kvs) =
  DA.Object $ Map.fromList $ map (\ (T.TString k,v) -> ( decodeUtf8 . B.toStrict $ k
                                                       , (mkJsonValue ps valTypeName v))) kvs

mkJsonMapValue ps keyTypeName valTypeName (T.TMap kType vType kvs) =
  DA.Object $ Map.fromList $ map (\ (k,v) -> ( pack $ show k
                                             , (mkJsonValue ps valTypeName v))) kvs

mkJsonMapValue ps keyTypeName valTypeName val = error $ "list type error" ++ show val



buildTypeValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> Text
               -> DA.Value
               -> T.ThriftVal
buildTypeValue ps typeName (DA.Object mapVal) =
  -- todo need to check the type and the val type for this build. Impl it later.
  -- this has the bug about ps need change
  buildStruct ps' decl  mapVal
  where (ps', decl) = (structDecl ps typeName)
buildTypeValue ps typeName v@(DA.Number b) = buildInt32Value v
buildTypeValue ps typeName val = error $ "struct or enmu type error" ++ show val



mkJsonObjectTypeValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                      -> Text
                      -> T.ThriftVal
                      -> DA.Value
                                                     -- this has the bug about the typeName has prefix then ps need change.
mkJsonObjectTypeValue ps typeName (T.TStruct mapVal) = mkJsonStruct ps' decl  mapVal
  where (ps', decl) = (structDecl ps typeName)
mkJsonObjectTypeValue ps typeName val = error $ "struct or enum type error" ++ show val


buildValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
           -> DA.Value
           -> T.ThriftVal
buildValue ps (LT.StringType _ _) v = buildStringValue v
buildValue ps (LT.BoolType _ _ ) v = buildBoolValue v
buildValue ps (LT.ByteType _ _) v =  buildByteValue v
buildValue ps (LT.I16Type _ _) v =  buildInt16Value v
buildValue ps (LT.I32Type _ _) v =  buildInt32Value v
buildValue ps (LT.I64Type _ _) v =  buildInt64Value v
buildValue ps (LT.DoubleType _ _) v =  buildDoubleValue v
buildValue ps (LT.ListType typeName _ _) v = buildListValue ps typeName v
buildValue ps (LT.MapType keyTypeName valTypeName _ _) v = buildMapValue ps keyTypeName valTypeName v
buildValue ps (LT.DefinedType typeName _) v = buildTypeValue ps typeName v


mkJsonValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
            -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
            -> T.ThriftVal
            -> DA.Value
mkJsonValue ps (LT.StringType _ _) v = mkJsonStringValue v
mkJsonValue ps (LT.BoolType _ _ ) v = mkJsonBoolValue v
mkJsonValue ps (LT.ByteType _ _) v =  mkJsonByteValue v
mkJsonValue ps (LT.I16Type _ _) v =  mkJsonInt16Value v
mkJsonValue ps (LT.I32Type _ _) v =  mkJsonInt32Value v
mkJsonValue ps (LT.I64Type _ _) v =  mkJsonInt64Value v
mkJsonValue ps (LT.DoubleType _ _) v =  mkJsonDoubleValue v
mkJsonValue ps (LT.ListType typeName _ _) v = mkJsonArrayValue ps typeName v
mkJsonValue ps (LT.MapType keyTypeName valTypeName _ _) v = mkJsonMapValue ps keyTypeName valTypeName v
mkJsonValue ps (LT.DefinedType typeName _) v = mkJsonObjectTypeValue ps typeName v


splitName :: Text -> (String, Text)
splitName name = let s = (splitOn "." $ unpack name)
                 in if length s == 2
                    then (head s, pack . head . tail $ s)
                    else ("", name)

-- merge this function.
buildField :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.Field Text.Megaparsec.Pos.SourcePos
           -> DA.Object -> Maybe (I.Int16, (L.Text, T.ThriftVal))
buildField ps f o = case mfval of Nothing-> Nothing
                                  Just fval -> Just (idx, ( L.fromStrict fname, buildValue ps ftype fval))
  where fname = LT.fieldName f
        ftype = LT.fieldValueType f
        idx = fromIntegral (M.fromJust $ LT.fieldIdentifier f) :: I.Int16
        mfval = Map.lookup fname o





-- merge this function.
mkField :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
        -> LT.Field Text.Megaparsec.Pos.SourcePos
        -> (L.Text, T.ThriftVal)
        -> DA.Pair
mkField ps f kv@(k, v) = (fname, mkJsonValue ps ftype v)
  where fname = LT.fieldName f
        ftype = LT.fieldValueType f


buildStruct :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
            -> LT.Struct Text.Megaparsec.Pos.SourcePos
            -> DA.Object
            -> T.ThriftVal
buildStruct ps decl obj = T.TStruct $ Map.fromList $ M.catMaybes buildFields
  where fields = LT.structFields decl
        buildFields :: [Maybe (I.Int16, (L.Text, T.ThriftVal))]
        buildFields = map (\f -> buildField ps f obj) fields


mkJsonStruct :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
             -> LT.Struct Text.Megaparsec.Pos.SourcePos
             -> Map.HashMap I.Int16 (L.Text, T.ThriftVal)
             -> DA.Value
mkJsonStruct ps decl m = DA.Object $ Map.fromList $ M.catMaybes buildFields
  where fields = LT.structFields decl
        buildFields :: [Maybe DA.Pair]
        buildFields = map (\f -> let idx = fromIntegral . M.fromJust . LT.fieldIdentifier $ f
                                     kv = Map.lookup idx m
                                 in case kv of
                                      Nothing -> Nothing
                                      Just kv' -> Just $ mkField ps f kv') fields


-- remove this function later.
structDecl :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> Text
           -> (Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos), LT.Struct Text.Megaparsec.Pos.SourcePos)
structDecl ps name = case findDecl ps name of
                       Just (Left decl) -> decl
                       Just (Right _) -> error $ "can not find struct" ++ show name
                       Nothing -> error $ "can not find struct" ++ show name

findDecl :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
         -> Text
         -> Maybe (Either
                    (Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos), LT.Struct Text.Megaparsec.Pos.SourcePos)
                    (Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos), LT.Enum Text.Megaparsec.Pos.SourcePos))
findDecl ps name = if length declList == 1
                     then Just $ head declList
                     else Nothing
  where
    (prefix, name') = splitName name
    p = M.fromJust $ Map.lookup prefix ps
    decls = map (\ (LT.TypeDefinition x) -> x) $
            filter (\ x -> case x of
                             LT.TypeDefinition _ -> True
                             _ -> False) $ LT.programDefinitions p

    declList = M.catMaybes $
                 map (\x -> case x of
                              (LT.StructType s) -> if LT.structName s == name'
                                                   then Just $ Left (Map.insert "" p ps, s)
                                                   else Nothing
                              (LT.EnumType s) -> if LT.enumName s == name'
                                                 then Just $ Right (Map.insert "" p ps, s)
                                                 else Nothing) decls





buildRequestParam :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                  -> LT.Field Text.Megaparsec.Pos.SourcePos
                  -> Map.HashMap Text DA.Value
                  -> (I.Int16, (L.Text, T.ThriftVal))
buildRequestParam ps field obj = let idx = fromIntegral (M.fromJust (LT.fieldIdentifier field))
                                     name = case LT.fieldValueType field of
                                              LT.DefinedType sName _ -> sName
                                              _ -> error "not impl type for "
                                     (ps', sd) = (structDecl ps name)
                                     tVal = buildStruct ps' sd (findObject (LT.fieldName field) obj)
                                 in (idx, (L.pack . unpack . removePrefix $ name, tVal))

  where
    findObject :: Text -> DA.Object -> DA.Object
    findObject k obj = case M.fromJust (Map.lookup k obj) of
                         DA.Object o -> o
                         _ -> error "error"


findServiceDecl :: LT.Program Text.Megaparsec.Pos.SourcePos
                -> Text
                -> LT.Service Text.Megaparsec.Pos.SourcePos
findServiceDecl p sName = let definitions  = map (\ (LT.ServiceDefinition x) -> x) $
                                             filter (\ x -> case x of
                                                        LT.ServiceDefinition _ -> True
                                                        _ -> False) $ LT.programDefinitions p
                              Just serviceDecl = find (\x -> LT.serviceName x == sName) definitions
                          in serviceDecl

buildRequest :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
             -> Text
             -> Text
             -> DA.Object
             -> T.ThriftVal
buildRequest ps sName fName obj = T.TStruct $ Map.fromList fields
  where
    p = M.fromJust (Map.lookup "" ps)
    serviceDecl = findServiceDecl p sName
    Just funcDecl = find (\x -> LT.functionName x == fName) $ LT.serviceFunctions serviceDecl
    fields = map (\x-> buildRequestParam ps x obj) $ LT.functionParameters funcDecl


buildTypeMap :: Text -> T.TypeMap
buildTypeMap name = undefined


buildResponse :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
              -> Text
              -> Text
              -> T.ThriftVal
              -> DA.Value
buildResponse ps sName fName tVal = mkJsonStruct ps' decl mapVal
  where
    p = M.fromJust (Map.lookup "" ps)
    serviceDecl = findServiceDecl p sName
    Just funcDecl = find (\x -> LT.functionName x == fName) $ LT.serviceFunctions serviceDecl
    Just (LT.DefinedType retName _) = LT.functionReturnType funcDecl
    (ps', decl) = structDecl ps retName
    T.TStruct mapVal = tVal


sendFunc :: (Protocol p, Protocol p, Transport t, Transport t) =>
            (p t, p t) ->
            Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos) ->
            Text ->
            Text ->
            DA.Object ->
            IO DA.Value
sendFunc (ip,op) ps sName fName vals = do
  seq <- seqid
  seqn <- R.readIORef seq
  let req = buildRequest ps sName fName vals
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
  let T.TStruct retVal  = case Map.lookup (0 :: I.Int16) mapVal of
                                      Just (_, T.TStruct v) -> T.TStruct v
                                      Just (_, v) -> error $ "unexcept response " ++ show v
                                      Nothing -> error $ "empty response " ++ show thriftVal
  return $ buildResponse ps sName fName (T.TStruct retVal)
