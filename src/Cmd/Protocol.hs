{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}


module Cmd.Protocol where

import Data.Connection
import qualified Data.HashMap.Strict as HS
import "thrift-haskell" Thrift.Protocol.Binary (binaryProtocol)
import "thrift-haskell" Thrift.Transport (openTransport)
import Data.Binary.Put (Put, runPut)
import Cmd.Types (buildRequest)
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T
import qualified Thrift as T
import qualified Thrift.Type as TH
import qualified Language.Thrift.AST as LT
import Data.Text (Text)
import qualified Text.Megaparsec.Pos
--import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA


sendFunc :: (TH.Protocol, TH.Transport) -> HS.HashMap String (LT.Program Text.Megaparsec.Pos.SourcePos) -> Text -> Text -> DA.Object -> IO ()
sendFunc (p, t) ps sName fName vals = do
  let req = buildRequest ps sName fName vals
  res <- TH.request sName False p t $ TH.fromTValue req
  print res
