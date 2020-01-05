{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Tasty
import Test.Tasty.HUnit


import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord
import Data.Text (Text, pack, unpack)
import qualified Data.Int as I
import qualified Thrift.Types as T
import qualified Data.Aeson as DA
import qualified Language.Thrift.Parser as LT
import qualified Data.Maybe as M (catMaybes, fromJust)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as Map
import Cmd.Lib



testBasic = testGroup "test build base request"
  [
    testCase "build basic struct" $ do
      let payload = "{\"msg\":\"hello\",\"xx\":\"hello1\",\"xx1\":{\"name\":\"gauss\",\"age\":12}}"
      Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
      let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
      let decl = structDecl p "DemoMessage"
      let v = buildStruct p decl $ M.fromJust jsonObject
      let expectStruct = T.TStruct (Map.fromList [ (1 :: I.Int16,("msg",T.TString (B.pack "hello")))
                                                 , (2 :: I.Int16,("xx",T.TString (B.pack "hello1")))])

      v @?= expectStruct

  , testCase "build basic struct" $ do
      let payload = "{\"str\":\"string\",\"flag\":true,\"b\":4,\"int16\":45,\"int32\":66,\"int64\":7877,\"d\":5.7}"
      Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
      let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
      let decl = structDecl p $ pack "BaseMessage"
      let v = buildStruct p decl $ M.fromJust jsonObject
      print v
      let expectStruct = T.TStruct (Map.fromList [ (1 :: I.Int16, ("str",T.TString (B.pack "string")))
                                                 , (2 :: I.Int16, ("flag",T.TBool True))
                                                 , (3 :: I.Int16, ("b",T.TByte 4))
                                                 , (4 :: I.Int16, ("int16",T.TI16 45))
                                                 , (5 :: I.Int16, ("int32",T.TI32 66))
                                                 , (6 :: I.Int16, ("int64",T.TI64 7877))
                                                 , (7 :: I.Int16, ("d",T.TDouble 5.7))])

      v @?= expectStruct
  ]



testBasicWithInner = testGroup "test build struct"
  [
    testCase "build basic struct" $ do
      let payload = "{\"outer_str\":\"outer\",\"inner\":{\"inner_str\":\"inner\",\"inner_double\":6.66},\"outer_double\":8.88}"
      Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
      let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
      let decl = structDecl p $ pack "Outer"
      let v = buildStruct p decl $ M.fromJust jsonObject
      let expectStruct = T.TStruct (Map.fromList [ (1 :: I.Int16,("outer_str",T.TString (B.pack "outer")))
                                                 , (2 :: I.Int16,("inner",T.TStruct
                                                                   (Map.fromList [ (1:: I.Int16, ("inner_str", T.TString (B.pack "inner")))
                                                                                 , (7:: I.Int16, ("inner_double", T.TDouble 6.66))])))
                                                 , (7 :: I.Int16,("outer_double",T.TDouble 8.88))
                                                 ])

      v @?= expectStruct

  ]




testBasic1 = testGroup "test build base request"
  [
    testCaseSteps "build request" $ \step -> do
      step "begin......"
      let payload1 = "{\"msg1\":{\"msg\":\"m1\",\"xx\":\"x1\"},\"msg2\":{\"msg\":\"m2\",\"xx\":\"x2\"}}"
      Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
      --let Right p = r
      let jsonObject = DA.decode $ B.pack payload1 :: Maybe DA.Object
      let d =  trace ("n: " ++ show jsonObject) jsonObject
      step (show jsonObject)
      let v = buildRequest p (pack "DemoService") (pack "demoFunction") $ M.fromJust jsonObject

      let expectStruct = T.TStruct (Map.fromList [ (1 :: I.Int16, ("msg1",T.TStruct (Map.fromList [ (1 :: I.Int16,("msg",T.TString "m1"))
                                                                                                  , (2 :: I.Int16,("xx",T.TString "x1"))])))
                                                 , (2 :: I.Int16, ("msg2",T.TStruct (Map.fromList [ (1 :: I.Int16,("msg",T.TString "m2"))
                                                                                                  , (2 :: I.Int16,("xx",T.TString "x2"))])))])

      v @?= expectStruct
  ]



devfun = do
  let payload = "{\"str\":\"string\",\"flag\":true,\"b\":4,\"int16\":45,\"int32\":66,\"int64\":7877,\"d\":5.7}"
  Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let decl = structDecl p $ pack "BaseMessage"
  let v = buildStruct p decl $ M.fromJust jsonObject
  print v


devfun1 = do
  let payload = "{\"outer_str\":\"outer\",\"inner\":{\"inner_str\":\"inner\",\"inner_double\":6.66},\"outer_double\":8.88}"
  Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let decl = structDecl p $ pack "Outer"
  let v = buildStruct p decl $ M.fromJust jsonObject
  print v

devfun2 = do
  let payload1 = "{\"msg1\":{\"msg\":\"m1\",\"xx\":\"x1\"},\"msg2\":{\"msg\":\"m2\",\"xx\":\"x2\"}}"
  Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-demo/IDL/demo.thrift"
  let jsonObject = DA.decode $ B.pack payload1 :: Maybe DA.Object
  let d =  trace ("n: " ++ show jsonObject) jsonObject
  let v = buildRequest p (pack "DemoService") (pack "demoFunction") $ M.fromJust jsonObject
  print v



devfun1 = do
  let payload = "{\"str\":\"string\",\"flag\":true,\"b\":4,\"int16\":45,\"int32\":66,\"int64\":7877,\"d\":5.7,\"strs\":[\"11\",\"22\",\"33\"],\"mstrs\":{\"aa\":\"bb\"}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let Just o = Map.lookup (pack "strs") $ M.fromJust jsonObject
  Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-cli/IDL/base.thrift"
  let DA.Array arr = o
  let decl = structDecl p $ pack "BaseMessage"
  let fs = LT.structFields decl
  let f = fs !! 7
  let f0 = fs !! 0
  let LT.ListType typeName _ _ = LT.fieldValueType f
  let v = map (\x -> buildValue p Map.empty typeName x) $ V.toList arr
  return f



devfun2 = do
  let payload = "{\"str\":\"string\",\"flag\":true,\"b\":4,\"int16\":45,\"int32\":66,\"int64\":7877,\"d\":5.7,\"strs\":[\"11\",\"22\",\"33\"],\"mstrs\":{\"aa\":\"bb\"}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let filePath = "/home/savior/data/github.repo/programming/haskell/thrift-cli/IDL/base.thrift"
  Right p <- LT.parseFromFile filePath
  let fileDir = takeDirectory filePath

  let decl = structDecl p $ pack "BaseMessage"
  let v = buildStruct p Map.empty decl $ M.fromJust jsonObject

  print v



devfun0 = do
  let payload = "{\"MediaId\":139,\"Metas\":[],\"Base\":{\"LogID\":\"00001\",\"Caller\":\"p.s.m\",\"Addr\":\"\",\"Client\":\"gauss.api\"}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let filePath = "/home/savior/go/src/code.byted.org/video/author_profile/idl/video/author_profile.thrift"
  -- let filePath = "/home/savior/go/src/code.byted.org/video/author_profile/idl/base.thrift"
  Right p <- LT.parseFromFile filePath
  let fileDir = takeDirectory filePath
  headerFilesPath' <- mapM (\ (LT.HeaderInclude h) -> do
                             let filePath = fileDir ++ "/" ++ (unpack $ LT.includePath h)
                             let fileName = takeBaseName filePath
                             Right decl <- LT.parseFromFile filePath
                             return (fileName, decl)
                         ) $ filter (\x -> case x of
                                             (LT.HeaderInclude _) -> True
                                             _ -> False) $ LT.programHeaders p
  let hm = Map.fromList headerFilesPath'
  let decl = structDecl p $ pack "GetAttrsRequest"

  let fields = LT.structFields decl
  let f = fields !! 2
  let v = buildStruct p hm decl $ M.fromJust jsonObject
  let Just obj = jsonObject

  let Just obj' = DA.decode $ B.pack "{\"LogID\":\"00001\",\"Caller\":\"p.s.m\",\"Addr\":\"\",\"Client\":\"gauss.api\"}" :: Maybe DA.Object

  let fname = LT.fieldName f
  let ftype = LT.fieldValueType f
  -- idx = fromIntegral (M.fromJust $ LT.fieldIdentifier f) :: I.Int16
  let mfval = Map.lookup fname obj
  let r = buildField p hm f obj
  print f
  return r

devfun3 = do
  let payload = "{\"req\":{\"MediaId\":139,\"Metas\":[],\"Base\":{\"LogID\":\"00001\",\"Caller\":\"p.s.m\",\"Addr\":\"\",\"Client\":\"gauss.api\"}}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object

  let filePath = "/home/savior/go/src/code.byted.org/video/author_profile/idl/video/author_profile.thrift"
  Right p <- LT.parseFromFile filePath
  let fileDir = takeDirectory filePath
  Right p <- LT.parseFromFile filePath
  headerFilesPath' <- mapM (\ (LT.HeaderInclude h) -> do
                             let filePath = fileDir ++ "/" ++ (unpack $ LT.includePath h)
                             let fileName = takeBaseName filePath
                             Right decl <- LT.parseFromFile filePath
                             return (fileName, decl)
                         ) $ filter (\x -> case x of
                                             (LT.HeaderInclude _) -> True
                                             _ -> False) $ LT.programHeaders p
  let hm = Map.fromList headerFilesPath'
  let v = buildRequest p hm (pack "AuthorProfileService") (pack "GetAttrs") $ M.fromJust jsonObject
  return v



devrespfun0 = do
  let payload = "{\"req\":{\"MediaId\":139,\"Metas\":[],\"Base\":{\"LogID\":\"00001\",\"Caller\":\"p.s.m\",\"Addr\":\"\",\"Client\":\"gauss.api\"}}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object

  let filePath = "/home/savior/go/src/code.byted.org/video/author_profile/idl/video/author_profile.thrift"
  Right p <- LT.parseFromFile filePath
  let fileDir = takeDirectory filePath
  Right p <- LT.parseFromFile filePath
  headerFilesPath' <- mapM (\ (LT.HeaderInclude h) -> do
                             let filePath = fileDir ++ "/" ++ (unpack $ LT.includePath h)
                             let fileName = takeBaseName filePath
                             Right decl <- LT.parseFromFile filePath
                             return (fileName, decl)
                         ) $ filter (\x -> case x of
                                             (LT.HeaderInclude _) -> True
                                             _ -> False) $ LT.programHeaders p
  let hm = Map.fromList headerFilesPath'
  let v = buildRequest p hm (pack "AuthorProfileService") (pack "GetAttrs") $ M.fromJust jsonObject
  return v




devfun9 = do
  let payload = "{\"str\":\"string\",\"flag\":true,\"b\":4,\"int16\":45,\"int32\":66,\"int64\":7877,\"d\":5.7,\"strs\":[\"11\",\"22\",\"33\"],\"mstrs\":{\"aa\":12}}"
  let jsonObject = DA.decode $ B.pack payload :: Maybe DA.Object
  let Just obj = Map.lookup (pack "mstrs") $ M.fromJust jsonObject
  Right p <- LT.parseFromFile "/home/savior/data/github.repo/programming/haskell/thrift-cli/IDL/base.thrift"
  let decl = structDecl p $ pack "BaseMessage"
  let fs = LT.structFields decl
  let f = fs !! 8
  let LT.MapType keyTypeName valTypeName _ _ = LT.fieldValueType f
  let v = buildValue p Map.empty (LT.fieldValueType f) obj
  print v
  return f



devfun0 = do
  let m1 = T.TStruct (Map.fromList [ (1 :: I.Int16,("msg", T.TString (B.pack "m1")))
                                   , (2 :: I.Int16,("xx",T.TString (B.pack "hello1")))])

  let m2 = T.TStruct (Map.fromList [ (1 :: I.Int16,("msg", T.TString (B.pack "m2")))
                                   , (2 :: I.Int16,("xx",T.TString (B.pack "hello1")))])

  let expectStruct = T.TStruct (Map.fromList [ ( 1 :: I.Int16
                                               , ("m1",T.TMap T.T_STRING T.T_I32 [( T.TString (B.pack "hello1")
                                                                                    , T.TI32 (32 :: I.Int32))]))
                                             , (2 :: I.Int16,("m2",T.TMap (T.T_STRUCT  $ buildTypeMap "k") (T.T_STRUCT  $ buildTypeMap "v") [(m1, m2)]))])

  Right p <- LT.parseFromFile "/home/savior/data/github.repo/thrift-cli/IDL/base.thrift"
  let ps = Map.insert "" p Map.empty
  let decl = structDecl ps $ pack "TestMessage"
  let T.TStruct mapVal = expectStruct
  let r = mkJsonStruct ps decl mapVal
  return r



devfun1 = do
  let m00 = T.TStruct (Map.fromList [ (1 :: I.Int16,("msg", T.TString (B.pack "m00")))
                                   , (2 :: I.Int16,("xx",T.TString (B.pack "hello1")))])

  let m01 = T.TStruct (Map.fromList [ (1 :: I.Int16,("msg", T.TString (B.pack "m01")))
                                   , (2 :: I.Int16,("xx",T.TString (B.pack "hello1")))])



  let m1 = T.TStruct (Map.fromList [ (1 :: I.Int16, ("ms"
                                                    , T.TList
                                                      (T.T_STRUCT $ buildTypeMap "k" )
                                                      [m00, m01]))])







  Right p <- LT.parseFromFile "/home/savior/data/github.repo/thrift-cli/IDL/base.thrift"
  let ps = Map.insert "" p Map.empty
  let decl = structDecl ps $ pack "Test2Message"
  let T.TStruct mapVal = m1
  let r = mkJsonStruct ps decl mapVal
  return r


tests :: TestTree
tests = testGroup "thrift-cli tests"
        [ testGroup "test basic" [testBasic, testBasicWithInner, testBasic1]
        ]




dddevfun2 = do
  let payload1 = "{\"req\": {\"msg\":\"aaaa\",\"msg2\":\"bbbb\"}}"

  let filePath = "/home/savior/data/github.repo/thrift-cli/IDL/service/demo.thrift"
  Right p <- LT.parseFromFile filePath
  let fileDir = takeDirectory filePath
  headerFilesPath' <- mapM (\ (LT.HeaderInclude h) -> do
                             let filePath = fileDir ++ "/" ++ (unpack $ LT.includePath h)
                             let fileName = takeBaseName filePath
                             Right decl <- LT.parseFromFile filePath
                             return (fileName, decl)
                         ) $ filter (\x -> case x of
                                             (LT.HeaderInclude _) -> True
                                             _ -> False) $ LT.programHeaders p
  let hm = Map.fromList headerFilesPath'
  let ps = Map.insert "" p hm
  let jsonObject = DA.decode $ B.pack payload1 :: Maybe DA.Object
  let sName = (pack "DemoService")
  let fName = (pack "fooFunction")
  let serviceDecl = findServiceDecl p sName
  let Just funcDecl = find (\x -> LT.functionName x == fName) $ LT.serviceFunctions serviceDecl
      --fields = map (\x-> buildField ps x obj) $ LT.functionParameters funcDecl
  --return $ LT.functionParameters funcDecl

  let v = buildRequest ps (pack "DemoService") (pack "fooFunction") $ M.fromJust jsonObject
  return v



main :: IO ()
main = undefined
