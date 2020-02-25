{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where


import Data.Text (pack, unpack)
import Cmd.Lib (buildRequest)
import Cmd.Protocol (sendFunc)
import qualified Data.Aeson as DA
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Language.Thrift.Parser as LT
import qualified Language.Thrift.AST as LT
import qualified Thrift.Types as T
import qualified Data.Maybe as M
import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Lazy as L
import qualified Text.Read hiding (read)
import qualified GHC.IO.Handle as H
import System.IO (IOMode(..))
import Network.Socket as NS
--import Thrift.Protocol.Binary
--import Thrift.Transport.Handle
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as P
import Options.Applicative.Help.Pretty ((.$.))
import Data.Monoid ((<>))
import Text.Regex.PCRE
import qualified Data.Int as I
import System.FilePath.Posix (takeDirectory, takeBaseName, FilePath)

import "thrift-haskell" Thrift.Protocol.Binary (binaryProtocol)
import "thrift-haskell" Thrift.Transport (openTransport)

data Args = Args
  { arguments :: String
  , payload :: String
  , payloadType :: String
  , dir :: Maybe String
  , file :: Maybe String
  } deriving (Show)


parseArgs :: Parser Args
parseArgs = Args
  <$> argument str (metavar "{protocol}//{ip}:{host}/{serviceName}/{functionName}")
  <*> strOption
          (metavar "PAYLOAD"
            <> long "data"
            <> short 'd'
            <> help "input payload" )
  <*> strOption
          (metavar "PAYLOAD_TYPE"
            <> long "payload_type"
            <> short 't'
            <> help "input payload type" )
  <*> optional (strOption
          (metavar "idl base dir"
            <> long "dir"
            <> help "thrift file dir" ))
  <*> optional (strOption
          (metavar "THRIFT FILE"
            <> long "file"
            <> help "thrift file" ))


parsePayload  :: String -> String -> Maybe DA.Object
parsePayload payload payloadType = DA.decode $ B.pack payload :: Maybe DA.Object

buildResponse :: Map.HashMap I.Int16 (L.Text, T.ThriftVal) -> DA.Object
buildResponse resp = undefined

main = do
  args@Args{..} <-execParser opts
  let [[_,prop, ip, port, sName, fName]] = arguments =~ ( "(\\w+)//(\\S+):(\\S+)/(\\S+)/(\\S+)" :: String) :: [[String]]
  let fileDir = takeDirectory $ M.fromJust file
  Right p <- LT.parseFromFile $ M.fromJust file
  headerFilesPath' <- mapM (\ (LT.HeaderInclude h) -> do
                               let filePath = fileDir ++ "/" ++ unpack (LT.includePath h)
                               let fileName = takeBaseName filePath
                               Right decl <- LT.parseFromFile filePath
                               return (fileName, decl)
                         ) $ filter (\x -> case x of
                                             (LT.HeaderInclude _) -> True
                                             _ -> False) $ LT.programHeaders p
  let hm = Map.fromList headerFilesPath'
  let ps = Map.insert "" p hm
  let jsonObject = parsePayload payload payloadType
  transport <- openTransport ip (read port :: PortNumber)
  result <- sendFunc (binaryProtocol, transport) ps (pack sName) (pack fName) $ M.fromJust jsonObject
  B.putStrLn $ encodePretty result

  where
    opts = info (helper <*> parseArgs)
      (fullDesc <> header "thrift cli")
