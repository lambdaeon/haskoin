module SimpleNode where


import           Conduit
import           Control.Concurrent             (threadDelay)
import           Control.Exception
import           Control.Monad                  (forever)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as C
import qualified Data.Conduit.Network           as CN
import           Data.Serializable
import qualified Data.Varint                    as Varint
import           Data.Varint                    (Varint (..))
import           Extension.ByteString.Parser
import qualified Network
import           Network.Common
import           Network.Run.TCP
import           Network.Socket                 (Socket, SockAddr)
import qualified Network.Socket                 as Socket
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Byte           as BP
import           Utils



run :: IO ()
run = do
  eithVerMsg <- runExceptT $ Network.makeVersionMsg Nothing Nothing
  case eithVerMsg of
    Right verMsg ->
      -- runTCPClient "72.48.253.168" "18333" $ \s -> do
      runTCPClient "testnet.programmingbitcoin.com" "18333" $ \s -> do
        print "---------- 1"
        sendAll s $ serialize $ Network.envelopeMessage True verMsg
        print "---------- 2"
        msg <- recv s 2048
        print "---------- 3"
        Socket.listen s 1
        putStr "Received: "
        C.putStrLn msg
    Left err ->
      fail $ show err



