{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.9.2)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module DemoService where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T

import qualified Base_Types


import Demo_Types
import qualified DemoService_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data DemoFunction_args = DemoFunction_args  { demoFunction_args_msg1 :: Base_Types.DemoMessage
  , demoFunction_args_msg2 :: Base_Types.BaseMessage
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable DemoFunction_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` demoFunction_args_msg1 record   `H.hashWithSalt` demoFunction_args_msg2 record  
instance QC.Arbitrary DemoFunction_args where 
  arbitrary = M.liftM DemoFunction_args (QC.arbitrary)
          `M.ap`(QC.arbitrary)
  shrink obj | obj == default_DemoFunction_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_DemoFunction_args{demoFunction_args_msg1 = demoFunction_args_msg1 obj} then P.Nothing else P.Just $ default_DemoFunction_args{demoFunction_args_msg1 = demoFunction_args_msg1 obj}
    , if obj == default_DemoFunction_args{demoFunction_args_msg2 = demoFunction_args_msg2 obj} then P.Nothing else P.Just $ default_DemoFunction_args{demoFunction_args_msg2 = demoFunction_args_msg2 obj}
    ]
from_DemoFunction_args :: DemoFunction_args -> T.ThriftVal
from_DemoFunction_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v14 -> P.Just (1, ("msg1",Base_Types.from_DemoMessage _v14))) $ demoFunction_args_msg1 record
  , (\_v14 -> P.Just (2, ("msg2",Base_Types.from_BaseMessage _v14))) $ demoFunction_args_msg2 record
  ]
write_DemoFunction_args :: (T.Protocol p, T.Transport t) => p t -> DemoFunction_args -> P.IO ()
write_DemoFunction_args oprot record = T.writeVal oprot $ from_DemoFunction_args record
encode_DemoFunction_args :: (T.Protocol p, T.Transport t) => p t -> DemoFunction_args -> LBS.ByteString
encode_DemoFunction_args oprot record = T.serializeVal oprot $ from_DemoFunction_args record
to_DemoFunction_args :: T.ThriftVal -> DemoFunction_args
to_DemoFunction_args (T.TStruct fields) = DemoFunction_args{
  demoFunction_args_msg1 = P.maybe (demoFunction_args_msg1 default_DemoFunction_args) (\(_,_val16) -> (case _val16 of {T.TStruct _val17 -> (Base_Types.to_DemoMessage (T.TStruct _val17)); _ -> P.error "wrong type"})) (Map.lookup (1) fields),
  demoFunction_args_msg2 = P.maybe (demoFunction_args_msg2 default_DemoFunction_args) (\(_,_val16) -> (case _val16 of {T.TStruct _val18 -> (Base_Types.to_BaseMessage (T.TStruct _val18)); _ -> P.error "wrong type"})) (Map.lookup (2) fields)
  }
to_DemoFunction_args _ = P.error "not a struct"
read_DemoFunction_args :: (T.Transport t, T.Protocol p) => p t -> P.IO DemoFunction_args
read_DemoFunction_args iprot = to_DemoFunction_args <$> T.readVal iprot (T.T_STRUCT typemap_DemoFunction_args)
decode_DemoFunction_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> DemoFunction_args
decode_DemoFunction_args iprot bs = to_DemoFunction_args $ T.deserializeVal iprot (T.T_STRUCT typemap_DemoFunction_args) bs
typemap_DemoFunction_args :: T.TypeMap
typemap_DemoFunction_args = Map.fromList [(1,("msg1",(T.T_STRUCT Base_Types.typemap_DemoMessage))),(2,("msg2",(T.T_STRUCT Base_Types.typemap_BaseMessage)))]
default_DemoFunction_args :: DemoFunction_args
default_DemoFunction_args = DemoFunction_args{
  demoFunction_args_msg1 = Base_Types.default_DemoMessage,
  demoFunction_args_msg2 = Base_Types.default_BaseMessage}
data DemoFunction_result = DemoFunction_result  { demoFunction_result_success :: Base_Types.DemoMessage
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable DemoFunction_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` demoFunction_result_success record  
instance QC.Arbitrary DemoFunction_result where 
  arbitrary = M.liftM DemoFunction_result (QC.arbitrary)
  shrink obj | obj == default_DemoFunction_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_DemoFunction_result{demoFunction_result_success = demoFunction_result_success obj} then P.Nothing else P.Just $ default_DemoFunction_result{demoFunction_result_success = demoFunction_result_success obj}
    ]
from_DemoFunction_result :: DemoFunction_result -> T.ThriftVal
from_DemoFunction_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v21 -> P.Just (0, ("success",Base_Types.from_DemoMessage _v21))) $ demoFunction_result_success record
  ]
write_DemoFunction_result :: (T.Protocol p, T.Transport t) => p t -> DemoFunction_result -> P.IO ()
write_DemoFunction_result oprot record = T.writeVal oprot $ from_DemoFunction_result record
encode_DemoFunction_result :: (T.Protocol p, T.Transport t) => p t -> DemoFunction_result -> LBS.ByteString
encode_DemoFunction_result oprot record = T.serializeVal oprot $ from_DemoFunction_result record
to_DemoFunction_result :: T.ThriftVal -> DemoFunction_result
to_DemoFunction_result (T.TStruct fields) = DemoFunction_result{
  demoFunction_result_success = P.maybe (demoFunction_result_success default_DemoFunction_result) (\(_,_val23) -> (case _val23 of {T.TStruct _val24 -> (Base_Types.to_DemoMessage (T.TStruct _val24)); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_DemoFunction_result _ = P.error "not a struct"
read_DemoFunction_result :: (T.Transport t, T.Protocol p) => p t -> P.IO DemoFunction_result
read_DemoFunction_result iprot = to_DemoFunction_result <$> T.readVal iprot (T.T_STRUCT typemap_DemoFunction_result)
decode_DemoFunction_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> DemoFunction_result
decode_DemoFunction_result iprot bs = to_DemoFunction_result $ T.deserializeVal iprot (T.T_STRUCT typemap_DemoFunction_result) bs
typemap_DemoFunction_result :: T.TypeMap
typemap_DemoFunction_result = Map.fromList [(0,("success",(T.T_STRUCT Base_Types.typemap_DemoMessage)))]
default_DemoFunction_result :: DemoFunction_result
default_DemoFunction_result = DemoFunction_result{
  demoFunction_result_success = Base_Types.default_DemoMessage}
data BarFunction_args = BarFunction_args  { barFunction_args_msg :: BarMessage
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable BarFunction_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` barFunction_args_msg record  
instance QC.Arbitrary BarFunction_args where 
  arbitrary = M.liftM BarFunction_args (QC.arbitrary)
  shrink obj | obj == default_BarFunction_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_BarFunction_args{barFunction_args_msg = barFunction_args_msg obj} then P.Nothing else P.Just $ default_BarFunction_args{barFunction_args_msg = barFunction_args_msg obj}
    ]
from_BarFunction_args :: BarFunction_args -> T.ThriftVal
from_BarFunction_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v27 -> P.Just (1, ("msg",from_BarMessage _v27))) $ barFunction_args_msg record
  ]
write_BarFunction_args :: (T.Protocol p, T.Transport t) => p t -> BarFunction_args -> P.IO ()
write_BarFunction_args oprot record = T.writeVal oprot $ from_BarFunction_args record
encode_BarFunction_args :: (T.Protocol p, T.Transport t) => p t -> BarFunction_args -> LBS.ByteString
encode_BarFunction_args oprot record = T.serializeVal oprot $ from_BarFunction_args record
to_BarFunction_args :: T.ThriftVal -> BarFunction_args
to_BarFunction_args (T.TStruct fields) = BarFunction_args{
  barFunction_args_msg = P.maybe (barFunction_args_msg default_BarFunction_args) (\(_,_val29) -> (case _val29 of {T.TStruct _val30 -> (to_BarMessage (T.TStruct _val30)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_BarFunction_args _ = P.error "not a struct"
read_BarFunction_args :: (T.Transport t, T.Protocol p) => p t -> P.IO BarFunction_args
read_BarFunction_args iprot = to_BarFunction_args <$> T.readVal iprot (T.T_STRUCT typemap_BarFunction_args)
decode_BarFunction_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> BarFunction_args
decode_BarFunction_args iprot bs = to_BarFunction_args $ T.deserializeVal iprot (T.T_STRUCT typemap_BarFunction_args) bs
typemap_BarFunction_args :: T.TypeMap
typemap_BarFunction_args = Map.fromList [(1,("msg",(T.T_STRUCT typemap_BarMessage)))]
default_BarFunction_args :: BarFunction_args
default_BarFunction_args = BarFunction_args{
  barFunction_args_msg = default_BarMessage}
data BarFunction_result = BarFunction_result  { barFunction_result_success :: BarMessage
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable BarFunction_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` barFunction_result_success record  
instance QC.Arbitrary BarFunction_result where 
  arbitrary = M.liftM BarFunction_result (QC.arbitrary)
  shrink obj | obj == default_BarFunction_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_BarFunction_result{barFunction_result_success = barFunction_result_success obj} then P.Nothing else P.Just $ default_BarFunction_result{barFunction_result_success = barFunction_result_success obj}
    ]
from_BarFunction_result :: BarFunction_result -> T.ThriftVal
from_BarFunction_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v33 -> P.Just (0, ("success",from_BarMessage _v33))) $ barFunction_result_success record
  ]
write_BarFunction_result :: (T.Protocol p, T.Transport t) => p t -> BarFunction_result -> P.IO ()
write_BarFunction_result oprot record = T.writeVal oprot $ from_BarFunction_result record
encode_BarFunction_result :: (T.Protocol p, T.Transport t) => p t -> BarFunction_result -> LBS.ByteString
encode_BarFunction_result oprot record = T.serializeVal oprot $ from_BarFunction_result record
to_BarFunction_result :: T.ThriftVal -> BarFunction_result
to_BarFunction_result (T.TStruct fields) = BarFunction_result{
  barFunction_result_success = P.maybe (barFunction_result_success default_BarFunction_result) (\(_,_val35) -> (case _val35 of {T.TStruct _val36 -> (to_BarMessage (T.TStruct _val36)); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_BarFunction_result _ = P.error "not a struct"
read_BarFunction_result :: (T.Transport t, T.Protocol p) => p t -> P.IO BarFunction_result
read_BarFunction_result iprot = to_BarFunction_result <$> T.readVal iprot (T.T_STRUCT typemap_BarFunction_result)
decode_BarFunction_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> BarFunction_result
decode_BarFunction_result iprot bs = to_BarFunction_result $ T.deserializeVal iprot (T.T_STRUCT typemap_BarFunction_result) bs
typemap_BarFunction_result :: T.TypeMap
typemap_BarFunction_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_BarMessage)))]
default_BarFunction_result :: BarFunction_result
default_BarFunction_result = BarFunction_result{
  barFunction_result_success = default_BarMessage}
data FooFunction_args = FooFunction_args  { fooFunction_args_msg :: Msg
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable FooFunction_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` fooFunction_args_msg record  
instance QC.Arbitrary FooFunction_args where 
  arbitrary = M.liftM FooFunction_args (QC.arbitrary)
  shrink obj | obj == default_FooFunction_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_FooFunction_args{fooFunction_args_msg = fooFunction_args_msg obj} then P.Nothing else P.Just $ default_FooFunction_args{fooFunction_args_msg = fooFunction_args_msg obj}
    ]
from_FooFunction_args :: FooFunction_args -> T.ThriftVal
from_FooFunction_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v39 -> P.Just (1, ("msg",from_Msg _v39))) $ fooFunction_args_msg record
  ]
write_FooFunction_args :: (T.Protocol p, T.Transport t) => p t -> FooFunction_args -> P.IO ()
write_FooFunction_args oprot record = T.writeVal oprot $ from_FooFunction_args record
encode_FooFunction_args :: (T.Protocol p, T.Transport t) => p t -> FooFunction_args -> LBS.ByteString
encode_FooFunction_args oprot record = T.serializeVal oprot $ from_FooFunction_args record
to_FooFunction_args :: T.ThriftVal -> FooFunction_args
to_FooFunction_args (T.TStruct fields) = FooFunction_args{
  fooFunction_args_msg = P.maybe (fooFunction_args_msg default_FooFunction_args) (\(_,_val41) -> (case _val41 of {T.TStruct _val42 -> (to_Msg (T.TStruct _val42)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_FooFunction_args _ = P.error "not a struct"
read_FooFunction_args :: (T.Transport t, T.Protocol p) => p t -> P.IO FooFunction_args
read_FooFunction_args iprot = to_FooFunction_args <$> T.readVal iprot (T.T_STRUCT typemap_FooFunction_args)
decode_FooFunction_args :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> FooFunction_args
decode_FooFunction_args iprot bs = to_FooFunction_args $ T.deserializeVal iprot (T.T_STRUCT typemap_FooFunction_args) bs
typemap_FooFunction_args :: T.TypeMap
typemap_FooFunction_args = Map.fromList [(1,("msg",(T.T_STRUCT typemap_Msg)))]
default_FooFunction_args :: FooFunction_args
default_FooFunction_args = FooFunction_args{
  fooFunction_args_msg = default_Msg}
data FooFunction_result = FooFunction_result  { fooFunction_result_success :: BarMessage
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable FooFunction_result where
  hashWithSalt salt record = salt   `H.hashWithSalt` fooFunction_result_success record  
instance QC.Arbitrary FooFunction_result where 
  arbitrary = M.liftM FooFunction_result (QC.arbitrary)
  shrink obj | obj == default_FooFunction_result = []
             | P.otherwise = M.catMaybes
    [ if obj == default_FooFunction_result{fooFunction_result_success = fooFunction_result_success obj} then P.Nothing else P.Just $ default_FooFunction_result{fooFunction_result_success = fooFunction_result_success obj}
    ]
from_FooFunction_result :: FooFunction_result -> T.ThriftVal
from_FooFunction_result record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v45 -> P.Just (0, ("success",from_BarMessage _v45))) $ fooFunction_result_success record
  ]
write_FooFunction_result :: (T.Protocol p, T.Transport t) => p t -> FooFunction_result -> P.IO ()
write_FooFunction_result oprot record = T.writeVal oprot $ from_FooFunction_result record
encode_FooFunction_result :: (T.Protocol p, T.Transport t) => p t -> FooFunction_result -> LBS.ByteString
encode_FooFunction_result oprot record = T.serializeVal oprot $ from_FooFunction_result record
to_FooFunction_result :: T.ThriftVal -> FooFunction_result
to_FooFunction_result (T.TStruct fields) = FooFunction_result{
  fooFunction_result_success = P.maybe (fooFunction_result_success default_FooFunction_result) (\(_,_val47) -> (case _val47 of {T.TStruct _val48 -> (to_BarMessage (T.TStruct _val48)); _ -> P.error "wrong type"})) (Map.lookup (0) fields)
  }
to_FooFunction_result _ = P.error "not a struct"
read_FooFunction_result :: (T.Transport t, T.Protocol p) => p t -> P.IO FooFunction_result
read_FooFunction_result iprot = to_FooFunction_result <$> T.readVal iprot (T.T_STRUCT typemap_FooFunction_result)
decode_FooFunction_result :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> FooFunction_result
decode_FooFunction_result iprot bs = to_FooFunction_result $ T.deserializeVal iprot (T.T_STRUCT typemap_FooFunction_result) bs
typemap_FooFunction_result :: T.TypeMap
typemap_FooFunction_result = Map.fromList [(0,("success",(T.T_STRUCT typemap_BarMessage)))]
default_FooFunction_result :: FooFunction_result
default_FooFunction_result = FooFunction_result{
  fooFunction_result_success = default_BarMessage}
process_demoFunction (seqid, iprot, oprot, handler) = do
  args <- read_DemoFunction_args iprot
  (X.catch
    (do
      val <- Iface.demoFunction handler (demoFunction_args_msg1 args) (demoFunction_args_msg2 args)
      let res = default_DemoFunction_result{demoFunction_result_success = val}
      T.writeMessageBegin oprot ("demoFunction", T.M_REPLY, seqid)
      write_DemoFunction_result oprot res
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot))
    ((\_ -> do
      T.writeMessageBegin oprot ("demoFunction", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_barFunction (seqid, iprot, oprot, handler) = do
  args <- read_BarFunction_args iprot
  (X.catch
    (do
      val <- Iface.barFunction handler (barFunction_args_msg args)
      let res = default_BarFunction_result{barFunction_result_success = val}
      T.writeMessageBegin oprot ("barFunction", T.M_REPLY, seqid)
      write_BarFunction_result oprot res
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot))
    ((\_ -> do
      T.writeMessageBegin oprot ("barFunction", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
process_fooFunction (seqid, iprot, oprot, handler) = do
  args <- read_FooFunction_args iprot
  (X.catch
    (do
      val <- Iface.fooFunction handler (fooFunction_args_msg args)
      let res = default_FooFunction_result{fooFunction_result_success = val}
      T.writeMessageBegin oprot ("fooFunction", T.M_REPLY, seqid)
      write_FooFunction_result oprot res
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot))
    ((\_ -> do
      T.writeMessageBegin oprot ("fooFunction", T.M_EXCEPTION, seqid)
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")
      T.writeMessageEnd oprot
      T.tFlush (T.getTransport oprot)) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "demoFunction" -> process_demoFunction (seqid,iprot,oprot,handler)
  "barFunction" -> process_barFunction (seqid,iprot,oprot,handler)
  "fooFunction" -> process_fooFunction (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessageBegin oprot (name,T.M_EXCEPTION,seqid)
    T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
    T.writeMessageEnd oprot
    T.tFlush (T.getTransport oprot)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- T.readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  T.readMessageEnd iprot
  P.return P.True
