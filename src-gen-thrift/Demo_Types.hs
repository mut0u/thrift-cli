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

module Demo_Types where
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


type JsonDict = LT.Text

type MsgNew = Msg

data Msg = Msg  { msg_msg :: LT.Text
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable Msg where
  hashWithSalt salt record = salt   `H.hashWithSalt` msg_msg record  
instance QC.Arbitrary Msg where 
  arbitrary = M.liftM Msg (QC.arbitrary)
  shrink obj | obj == default_Msg = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Msg{msg_msg = msg_msg obj} then P.Nothing else P.Just $ default_Msg{msg_msg = msg_msg obj}
    ]
from_Msg :: Msg -> T.ThriftVal
from_Msg record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v2 -> P.Just (1, ("msg",T.TString $ E.encodeUtf8 _v2))) $ msg_msg record
  ]
write_Msg :: (T.Protocol p, T.Transport t) => p t -> Msg -> P.IO ()
write_Msg oprot record = T.writeVal oprot $ from_Msg record
encode_Msg :: (T.Protocol p, T.Transport t) => p t -> Msg -> LBS.ByteString
encode_Msg oprot record = T.serializeVal oprot $ from_Msg record
to_Msg :: T.ThriftVal -> Msg
to_Msg (T.TStruct fields) = Msg{
  msg_msg = P.maybe (msg_msg default_Msg) (\(_,_val4) -> (case _val4 of {T.TString _val5 -> E.decodeUtf8 _val5; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Msg _ = P.error "not a struct"
read_Msg :: (T.Transport t, T.Protocol p) => p t -> P.IO Msg
read_Msg iprot = to_Msg <$> T.readVal iprot (T.T_STRUCT typemap_Msg)
decode_Msg :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> Msg
decode_Msg iprot bs = to_Msg $ T.deserializeVal iprot (T.T_STRUCT typemap_Msg) bs
typemap_Msg :: T.TypeMap
typemap_Msg = Map.fromList [(1,("msg",T.T_STRING))]
default_Msg :: Msg
default_Msg = Msg{
  msg_msg = ""}
data BarMessage = BarMessage  { barMessage_msg :: LT.Text
  } deriving (P.Show,P.Eq,TY.Typeable)
instance H.Hashable BarMessage where
  hashWithSalt salt record = salt   `H.hashWithSalt` barMessage_msg record  
instance QC.Arbitrary BarMessage where 
  arbitrary = M.liftM BarMessage (QC.arbitrary)
  shrink obj | obj == default_BarMessage = []
             | P.otherwise = M.catMaybes
    [ if obj == default_BarMessage{barMessage_msg = barMessage_msg obj} then P.Nothing else P.Just $ default_BarMessage{barMessage_msg = barMessage_msg obj}
    ]
from_BarMessage :: BarMessage -> T.ThriftVal
from_BarMessage record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v8 -> P.Just (1, ("msg",T.TString $ E.encodeUtf8 _v8))) $ barMessage_msg record
  ]
write_BarMessage :: (T.Protocol p, T.Transport t) => p t -> BarMessage -> P.IO ()
write_BarMessage oprot record = T.writeVal oprot $ from_BarMessage record
encode_BarMessage :: (T.Protocol p, T.Transport t) => p t -> BarMessage -> LBS.ByteString
encode_BarMessage oprot record = T.serializeVal oprot $ from_BarMessage record
to_BarMessage :: T.ThriftVal -> BarMessage
to_BarMessage (T.TStruct fields) = BarMessage{
  barMessage_msg = P.maybe (barMessage_msg default_BarMessage) (\(_,_val10) -> (case _val10 of {T.TString _val11 -> E.decodeUtf8 _val11; _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_BarMessage _ = P.error "not a struct"
read_BarMessage :: (T.Transport t, T.Protocol p) => p t -> P.IO BarMessage
read_BarMessage iprot = to_BarMessage <$> T.readVal iprot (T.T_STRUCT typemap_BarMessage)
decode_BarMessage :: (T.Protocol p, T.Transport t) => p t -> LBS.ByteString -> BarMessage
decode_BarMessage iprot bs = to_BarMessage $ T.deserializeVal iprot (T.T_STRUCT typemap_BarMessage) bs
typemap_BarMessage :: T.TypeMap
typemap_BarMessage = Map.fromList [(1,("msg",T.T_STRING))]
default_BarMessage :: BarMessage
default_BarMessage = BarMessage{
  barMessage_msg = ""}
