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

module DemoService_Client(demoFunction,barFunction,fooFunction) where
import qualified Data.IORef as R
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
import DemoService
seqid = R.newIORef 0
demoFunction (ip,op) arg_msg1 arg_msg2 = do
  send_demoFunction op arg_msg1 arg_msg2
  recv_demoFunction ip
send_demoFunction op arg_msg1 arg_msg2 = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("demoFunction", T.M_CALL, seqn)
  write_DemoFunction_args op (DemoFunction_args{demoFunction_args_msg1=arg_msg1,demoFunction_args_msg2=arg_msg2})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_demoFunction ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_DemoFunction_result ip
  T.readMessageEnd ip
  P.return $ demoFunction_result_success res
barFunction (ip,op) arg_msg = do
  send_barFunction op arg_msg
  recv_barFunction ip
send_barFunction op arg_msg = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("barFunction", T.M_CALL, seqn)
  write_BarFunction_args op (BarFunction_args{barFunction_args_msg=arg_msg})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_barFunction ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_BarFunction_result ip
  T.readMessageEnd ip
  P.return $ barFunction_result_success res
fooFunction (ip,op) arg_msg = do
  send_fooFunction op arg_msg
  recv_fooFunction ip
send_fooFunction op arg_msg = do
  seq <- seqid
  seqn <- R.readIORef seq
  T.writeMessageBegin op ("fooFunction", T.M_CALL, seqn)
  write_FooFunction_args op (FooFunction_args{fooFunction_args_msg=arg_msg})
  T.writeMessageEnd op
  T.tFlush (T.getTransport op)
recv_fooFunction ip = do
  (fname, mtype, rseqid) <- T.readMessageBegin ip
  M.when (mtype == T.M_EXCEPTION) $ do { exn <- T.readAppExn ip ; T.readMessageEnd ip ; X.throw exn }
  res <- read_FooFunction_result ip
  T.readMessageEnd ip
  P.return $ fooFunction_result_success res
