{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}


module Cmd.Protocol where

import Data.Connection
import qualified Data.HashMap.Strict as HS
import "thrift-haskell" Thrift.Protocol.Binary (binaryProtocol)
import "thrift-haskell" Thrift.Transport (openTransport)
import Data.Binary.Put (Put, runPut)
import Cmd.Types (buildRequest, buildResponse)
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T
import qualified Thrift as T
import qualified Thrift.Type as TH
import qualified Language.Thrift.AST as LT
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified Data.IntMap as IM
import qualified Text.Megaparsec.Pos
--import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA


sendFunc :: (TH.Protocol, TH.Transport) -> HS.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos) -> Text -> Text -> DA.Object -> IO DA.Value
sendFunc (p, t) ps sName fName vals = do
  let req = buildRequest ps sName fName vals
  TH.TStruct res <- TH.rawFuncCall fName p t req
  let s = fromJust $ IM.lookup 0 (TH.mkIntMap res)
  liftIO $ print s
  return $ buildResponse ps sName fName s
