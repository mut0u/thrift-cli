{-# LANGUAGE PackageImports #-}
-- |

module Cmd.Types where

import qualified Data.IORef as R
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict((!))
import qualified Data.Int as I
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V
import qualified Thrift.Type as Thrift
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as Bs
import qualified Language.Thrift.Parser as LT
import qualified Language.Thrift.AST as LT
import qualified Data.Maybe as M (catMaybes, fromJust)
import qualified Data.Scientific as S
import Data.List (find)
import Control.Monad
import System.IO
import qualified Control.Monad as M ( liftM, ap, when )
import qualified Control.Exception as X
import Data.List.Split (splitOn)
import qualified Text.Megaparsec.Pos
import Data.Binary.Put
import Data.Binary.Get
import System.FilePath.Posix (takeDirectory, takeBaseName, FilePath)


-- remove field name prefix like a.b to b
removePrefix :: Text -> Text
removePrefix name = let ns = (splitOn "." $ unpack name)
                    in case length ns of
                         2 -> pack . head . init $ ns
                         _ -> name

typeTransformer (LT.StringType _ _) = Thrift.TC_Binary
typeTransformer (LT.BinaryType _ _) = Thrift.TC_Binary
typeTransformer (LT.BoolType _ _ )  = Thrift.TC_Bool
typeTransformer (LT.ByteType _ _) = Thrift.TC_Int16
typeTransformer (LT.I16Type _ _) = Thrift.TC_Int16
typeTransformer (LT.I32Type _ _) = Thrift.TC_Int32
typeTransformer (LT.I64Type _ _) = Thrift.TC_Int64
typeTransformer (LT.DoubleType _ _) = Thrift.TC_Double
typeTransformer _ = error "can not find type"



buildStringValue :: DA.Value -> Thrift.TValue
buildStringValue (DA.String s) = Thrift.toTValue $ encodeUtf8 s
buildStringValue v = error $ "string type error" ++ show v

mkJsonStringValue :: Thrift.TValue -> DA.Value
mkJsonStringValue (Thrift.TBinary s) = DA.String (decodeUtf8 s)
mkJsonStringValue v = error $ "string type error" ++ show v


buildBoolValue :: DA.Value -> Thrift.TValue
buildBoolValue (DA.Bool b) =  Thrift.toTValue b
buildBoolValue v = error $ "bool type error" ++ show v

mkJsonBoolValue :: Thrift.TValue -> DA.Value
mkJsonBoolValue (Thrift.TBool b) =  DA.Bool b
mkJsonBoolValue v = error $ "bool type error" ++ show v


buildByteValue :: DA.Value -> Thrift.TValue
buildByteValue (DA.Number b) = case S.floatingOrInteger b of
                                 Right i -> Thrift.toTValue (fromIntegral i :: I.Int8)
                                 Left _ -> error $ "byte type error" ++ show b
buildByteValue v = error $ "byte type error" ++ show v


mkJsonByteValue :: Thrift.TValue -> DA.Value
mkJsonByteValue (Thrift.TInt8 b) = DA.Number (read (show b) :: S.Scientific)
mkJsonByteValue v = error $ "byte type error" ++ show v


buildInt16Value :: DA.Value -> Thrift.TValue
buildInt16Value (DA.Number b) = case S.floatingOrInteger b of
                                       Right i -> Thrift.toTValue (fromIntegral i :: I.Int16)
                                       Left _ -> error $ "int16 type error" ++ show b
buildInt16Value v = error $ "int16 type error" ++ show v


-- todo fix the integer parse to float.
mkJsonInt16Value :: Thrift.TValue -> DA.Value
mkJsonInt16Value (Thrift.TInt16 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt16Value v = error $ "int16 type error" ++ show v


buildInt32Value :: DA.Value -> Thrift.TValue
buildInt32Value (DA.Number b) = case S.floatingOrInteger b of
                                  Right i -> Thrift.toTValue (fromIntegral i :: I.Int32)
                                  Left _ -> error $ "int32 type error" ++ show b
buildInt32Value v = error $ "int32 type error" ++ show v

mkJsonInt32Value :: Thrift.TValue -> DA.Value
mkJsonInt32Value (Thrift.TInt32 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt32Value v = error $ "int32 type error" ++ show v

buildInt64Value :: DA.Value -> Thrift.TValue
buildInt64Value (DA.Number b) = case S.floatingOrInteger b of
                                  Right i -> Thrift.toTValue (fromIntegral i :: I.Int64)
                                  Left _ -> error $ "int64 type error" ++ show b
buildInt64Value v = error $ "int64 type error" ++ show v

mkJsonInt64Value :: Thrift.TValue-> DA.Value
mkJsonInt64Value (Thrift.TInt64 i) = DA.Number (read (show i) :: S.Scientific)
mkJsonInt64Value v = error $ "int64 type error" ++ show v

buildDoubleValue :: DA.Value -> Thrift.TValue
buildDoubleValue (DA.Number b) = case S.floatingOrInteger b of
                                       Right _ ->  error $ "double type error" ++ show b
                                       Left d -> Thrift.toTValue (d :: Double)
buildDoubleValue v = error $ "int64 type error" ++ show v

mkJsonDoubleValue :: Thrift.TValue -> DA.Value
mkJsonDoubleValue (Thrift.TDouble i) = DA.Number (read (show i) :: S.Scientific)
mkJsonDoubleValue v = error $ "double type error" ++ show v


buildListValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> DA.Value
               -> Thrift.TValue
buildListValue ps typeName (DA.Array arr) =  Thrift.TList (typeTransformer typeName) $ map (buildValue ps typeName) $ V.toList arr
buildListValue ps typeName v = error $ "list type error: type" ++ show typeName ++ " value :" ++ show v


mkJsonArrayValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                 -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
                 -> Thrift.TValue
                 -> DA.Value
mkJsonArrayValue ps typeName (Thrift.TList _ vals) =  DA.Array (V.fromList $ map (mkJsonValue ps typeName) vals)
mkJsonArrayValue ps typeName v = error $ "list type error" ++ show v


buildMapValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
              -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
              -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
              -> DA.Value
              -> Thrift.TValue
buildMapValue ps keyTypeName valTypeName (DA.Object m) =
  let kvThriftVals = map (\ (k,v) -> ( buildValue ps keyTypeName $ DA.String k
                                     , buildValue ps valTypeName v)) $ Map.toList m
  in Thrift.TMap (typeTransformer keyTypeName) (typeTransformer valTypeName) kvThriftVals

buildMapValue ps keyTypeName valTypeName val = error $ "list type error" ++ show val


mkJsonMapValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
               -> Thrift.TValue
               -> DA.Value
mkJsonMapValue ps (LT.StringType _ _) valTypeName (Thrift.TMap kType vType kvs) =
  DA.Object $ Map.fromList $ map (\ (k,v) -> ( Thrift.fromTValue k
                                             , mkJsonValue ps valTypeName v)) kvs

mkJsonMapValue ps keyTypeName valTypeName (Thrift.TMap kType vType kvs) =
  DA.Object $ Map.fromList $ map (\ (k,v) -> ( pack $ show k
                                             , mkJsonValue ps valTypeName v)) kvs

mkJsonMapValue ps keyTypeName valTypeName val = error $ "list type error" ++ show val

buildDefinedTypeValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                      -> LT.Type Text.Megaparsec.Pos.SourcePos
                      -> Text
                      -> DA.Value
                      -> Thrift.TValue
buildDefinedTypeValue ps (LT.StructType decl) typeName v@(DA.Object mapVal) = buildStruct ps decl mapVal
buildDefinedTypeValue ps (LT.TypedefType defType) typeName v = buildValue ps (LT.typedefTargetType defType) v
buildDefinedTypeValue ps (LT.EnumType _) typeName v@(DA.Number b) = buildInt32Value v
buildDefinedTypeValue ps _ typeName val = error $ "struct or enum type error" ++ show val


mkJsonObjectTypeValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                      -> LT.Type Text.Megaparsec.Pos.SourcePos
                      -> Text
                      -> Thrift.TValue
                      -> DA.Value
mkJsonObjectTypeValue ps (LT.StructType decl) typeName (Thrift.TStruct mapVal) = mkJsonStruct ps decl $ Map.fromList mapVal
mkJsonObjectTypeValue ps (LT.TypedefType defType) typeName v = mkJsonValue ps (LT.typedefTargetType defType) v
mkJsonObjectTypeValue ps (LT.EnumType _) typeName v = mkJsonInt32Value v
mkJsonObjectTypeValue ps _ typeName val = error $ "struct or enum type error" ++ show val


buildValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
           -> DA.Value
           -> Thrift.TValue
buildValue ps (LT.StringType _ _) v = buildStringValue v
buildValue ps (LT.BinaryType _ _) v = buildStringValue v
buildValue ps (LT.BoolType _ _ ) v = buildBoolValue v
buildValue ps (LT.ByteType _ _) v =  buildByteValue v
buildValue ps (LT.I16Type _ _) v =  buildInt16Value v
buildValue ps (LT.I32Type _ _) v =  buildInt32Value v
buildValue ps (LT.I64Type _ _) v =  buildInt64Value v
buildValue ps (LT.DoubleType _ _) v =  buildDoubleValue v
buildValue ps (LT.ListType typeName _ _) v = buildListValue ps typeName v
buildValue ps (LT.MapType keyTypeName valTypeName _ _) v = buildMapValue ps keyTypeName valTypeName v
buildValue ps (LT.DefinedType typeName _) v = case findDecl ps typeName of
                                                Just (ps', t) -> buildDefinedTypeValue ps' t typeName v
                                                Nothing -> error $ show "can not build type " ++ show typeName


mkJsonValue :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
            -> LT.TypeReference Text.Megaparsec.Pos.SourcePos
            -> Thrift.TValue
            -> DA.Value
mkJsonValue ps (LT.StringType _ _) v = mkJsonStringValue v
mkJsonValue ps (LT.BinaryType _ _) v = mkJsonStringValue v
mkJsonValue ps (LT.BoolType _ _ ) v = mkJsonBoolValue v
mkJsonValue ps (LT.ByteType _ _) v =  mkJsonByteValue v
mkJsonValue ps (LT.I16Type _ _) v =  mkJsonInt16Value v
mkJsonValue ps (LT.I32Type _ _) v =  mkJsonInt32Value v
mkJsonValue ps (LT.I64Type _ _) v =  mkJsonInt64Value v
mkJsonValue ps (LT.DoubleType _ _) v =  mkJsonDoubleValue v
mkJsonValue ps (LT.ListType typeName _ _) v = mkJsonArrayValue ps typeName v
mkJsonValue ps (LT.MapType keyTypeName valTypeName _ _) v = mkJsonMapValue ps keyTypeName valTypeName v
mkJsonValue ps (LT.DefinedType typeName _) v = -- mkJsonObjectTypeValue ps typeName v
  case findDecl ps typeName of
    Just (ps', t) -> mkJsonObjectTypeValue ps' t typeName v
    Nothing -> error $ show "can not build type " ++ show typeName

splitName :: Text -> (String, Text)
splitName name = let s = (splitOn "." $ unpack name)
                 in if length s == 2
                    then (head s, pack . head . tail $ s)
                    else ("", name)

-- merge this function.
buildField :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
           -> LT.Field Text.Megaparsec.Pos.SourcePos
           -> DA.Object -> Maybe (Int, Thrift.TValue)
buildField ps f o = case mfval of Nothing-> Nothing
                                  Just fval -> Just (idx, buildValue ps ftype fval)
  where fname = LT.fieldName f
        ftype = LT.fieldValueType f
        idx = fromIntegral (M.fromJust $ LT.fieldIdentifier f) :: Int
        mfval = Map.lookup fname o


-- merge this function.
mkField :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
        -> LT.Field Text.Megaparsec.Pos.SourcePos
        -> Thrift.TValue
        -> DA.Pair
mkField ps f v = (fname, mkJsonValue ps ftype v)
  where fname = LT.fieldName f
        ftype = LT.fieldValueType f


buildStruct :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
            -> LT.Struct Text.Megaparsec.Pos.SourcePos
            -> DA.Object
            -> Thrift.TValue
buildStruct ps decl obj = Thrift.TStruct $ M.catMaybes buildFields
  where fields = LT.structFields decl
        buildFields :: [Maybe (Int, Thrift.TValue)]
        buildFields = map (\f -> buildField ps f obj) fields


mkJsonStruct :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
             -> LT.Struct Text.Megaparsec.Pos.SourcePos
             -> Map.HashMap Int Thrift.TValue
             -> DA.Value
mkJsonStruct ps decl m = DA.Object $ Map.fromList $ M.catMaybes buildFields
  where fields = LT.structFields decl
        buildFields :: [Maybe DA.Pair]
        buildFields = map (\f -> let idx = fromIntegral . M.fromJust . LT.fieldIdentifier $ f
                                     v = Map.lookup idx m
                                 in case v of
                                      Nothing -> Nothing
                                      Just v' -> Just $ mkField ps f v') fields


findDecl :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
         -> Text
         -> Maybe (Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos), LT.Type Text.Megaparsec.Pos.SourcePos)
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
               -- maybe can use lens function
                 map (\x -> case x of
                              x1@(LT.StructType s) -> if LT.structName s == name'
                                                      then Just (Map.insert "" p ps, x1)
                                                      else Nothing
                              x1@(LT.EnumType s) -> if LT.enumName s == name'
                                                    then Just (Map.insert "" p ps, x1)
                                                    else Nothing
                              x1@(LT.TypedefType s) -> if LT.typedefName s == name'
                                                       then Just (Map.insert "" p ps, x1)
                                                       else Nothing

                              _ -> error $ "can not parse the type " ++ show x
                     ) decls



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
             -> Thrift.TValue
buildRequest ps sName fName obj = Thrift.TStruct $ M.catMaybes fields
  where
    fields = map (\x-> buildField ps x obj) $ LT.functionParameters (findCurrentFuncDecl ps sName fName)


--buildTypeMap :: Text -> T.TypeMap
--buildTypeMap name = undefined


buildResponse :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
              -> Text
              -> Text
              -> Thrift.TValue
              -> DA.Value
buildResponse ps sName fName tVal = mkJsonStruct ps' decl $ Map.fromList mapVal
  where
    structDecl :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
               -> Text
               -> (Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos), LT.Struct Text.Megaparsec.Pos.SourcePos)
    structDecl ps name = case findDecl ps name of
                       Just (ps, LT.StructType decl) -> (ps, decl)
                       Nothing -> error $ "can not find struct" ++ show name
    Just (LT.DefinedType retName _) = LT.functionReturnType (findCurrentFuncDecl ps sName fName)
    (ps', decl) = structDecl ps retName
    Thrift.TStruct mapVal = tVal


findCurrentFuncDecl :: Map.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos)
                    -> Text
                    -> Text
                    -> LT.Function Text.Megaparsec.Pos.SourcePos
findCurrentFuncDecl ps sName fName = let p = M.fromJust (Map.lookup "" ps)
                                         serviceDecl = findServiceDecl p sName
                                         Just funcDecl = find (\x -> LT.functionName x == fName) $ LT.serviceFunctions serviceDecl
                                     in funcDecl
