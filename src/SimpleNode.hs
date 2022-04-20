module SimpleNode where


import           Conduit
import           Control.Concurrent             (threadDelay)
import           Control.Exception
import           Control.Monad                  (forever)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Conduit.Network           as CN
import           Data.Serializable
import qualified Data.Varint                    as Varint
import           Data.Varint                    (Varint (..))
import           Extension.ByteString.Parser
import qualified Network
import           Network.Common
import           Network.Socket                 (Socket, SockAddr)
import qualified Network.Socket                 as Socket
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Byte           as BP
import           Utils


getTestnetAddrInfo :: IO [Socket.AddrInfo]
getTestnetAddrInfo =
  Socket.getAddrInfo
    Nothing
    (Just "testnet.programmingbitcoin.com")
    Nothing
    -- (Just "18333")


getSocket :: IO Socket --      Protocol number for TCP v--v
getSocket = Socket.socket Socket.AF_INET Socket.Stream 0x06


sockAddr :: Socket.PortNumber -> IP -> SockAddr
sockAddr portNo (IPv4 b0 b1 b2 b3) =
  -- {{{
  Socket.SockAddrInet portNo $ Socket.tupleToHostAddress (b0, b1, b2, b3)
  -- }}}
sockAddr portNo (IPv6 bs0 bs1 bs2 bs3 bs4 bs5 bs6 bs7) =
  -- {{{
  Socket.SockAddrInet6
    portNo
    0
    (Socket.tupleToHostAddress6 (bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7))
    0
  -- SockAddrInet6 !PortNumber !FlowInfo !HostAddress6 !ScopeID
  -- }}}


sourceSocket :: MonadIO m => ConduitT i BS.ByteString m ()
sourceSocket = do
  s <- liftIO getSocket
  CN.sourceSocket s

connectTest = do
  sock <- getSocket
  Socket.connect sock (sockAddr 18333 $ IPv4 72 48 253 168)

clientSettings = CN.clientSettings 18333 "testnet.programmingbitcoin.com"




handshake :: Socket -> IO ()
handshake sock = do
  (soc, _)   <- Socket.accept sock
  eithVerMsg <- runExceptT $ Network.makeVersionMsg Nothing Nothing
  case eithVerMsg of
    Right verMsg -> do
      sendAll soc $ Network.serializeMessage verMsg
    Left err ->
      fail $ show err
  Socket.close soc




run :: IO ()
run = Socket.withSocketsDo $ do
  addrInfo : _ <- getTestnetAddrInfo
  sock <- Socket.socket (Socket.addrFamily addrInfo) Socket.Stream 0x06
  Socket.bind sock (Socket.addrAddress addrInfo)
  Socket.listen sock 1
  handshake sock
  Socket.close sock



