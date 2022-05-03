module SimpleNode where


import qualified Block
import           Conduit
import           Control.Concurrent             (threadDelay)
import           Control.Exception
import           Control.Monad                  (forever, unless)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as C
import qualified Data.Conduit.Network           as CN
import           Data.Serializable
import qualified Data.Text                      as T
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



-- | The provided server from the book doesn't seem to be performing
--   properly, therefore I decided to spin up a rudimentary version
--   of my own based on the procedure described in the book.
--
--   This action starts by listening to port 18333 (testnet) for
--   a valid @version@ message. When it receives one, it sends back
--   the enveloped @verack@ message first, and its own @version@
--   message (it's better to wrap the two packets into an
--   intermediate datatype).
--
--   At this point it blocks and keeps listening for client's @verack@
--   message. The handshake completes when the envelope arrives.
server :: IO ()
server =
  -- {{{
  let
    handshake s = do
      -- {{{
      msg <- recv s 1024
      case parseVersion msg of
        Right (Network.Version verInfo) -> do
          -- {{{
          putStrLn "Received a valid Version message."
          putStrLn "Sending a VerAck message..."
          sendVerAck s
          putStrLn "Done."
          putStrLn "Sending the Version message..."
          sendVersion s
          putStrLn "Done."
          putStrLn "Waiting for a VerAck."
          handshake s
          -- }}}
        Left _                          ->
          -- {{{
          case parseVerAck msg of
            Right (Network.VerAck _) -> do
              -- {{{
              putStrLn "Received a valid VerAck message."
              putStrLn "Handshake completed."
              -- }}}
            Left _                   ->
              -- {{{
              fail "invalid version packet."
              -- }}}
          -- }}}
      -- }}}
  in do
  putStrLn "\n\n~~~~ Bitcoin Node Server running on port 18333 ~~~~\n"
  runTCPServer Nothing "18333" handshake
  -- }}}


-- | The `client` action performs the handshake as
--   described in the book:
--   
--   (1) Starts by sending its enveloped @version@ message.
--   
--   2.  Waits to receive two packets: first a @verack@ message,
--       and next, server's @version@ message.
--
--   3.  In case of a successful parse of the envelopes, it sends
--       a @verack@ message to complete the handshake.
client :: IO ()
client = do
  -- {{{
  -- runTCPClient "72.48.253.168" "18333" $ \s -> do
  -- runTCPClient "mainnet.programmingbitcoin.com" "8333" $ \s -> do
  runTCPClient "127.0.0.1" "18333" $ \s -> do
    putStrLn "Sending the Version message..."
    sendVersion s
    putStrLn "Done."
    putStrLn "Waiting for a VerAck message..."
    verAckRes  <- recv s 1024
    putStr "First packet received: "
    print $ encodeHex verAckRes
    putStrLn "Waiting for a Version message..."
    versionRes <- recv s 1024
    putStr "Second packet received: "
    print $ encodeHex versionRes
    case (parseVerAck verAckRes, parseVersion versionRes) of
      (Right (Network.VerAck _), Right (Network.Version verInfo)) -> do
        -- {{{
        putStrLn "Received valid packets."
        putStrLn "Sending the VerAck message..."
        sendVerAck s
        putStrLn "Done."
        -- }}}
      _ ->
        -- {{{
        fail "invalid response for handshake."
        -- }}}
  -- }}}


-- | Requests maximum number of block heads (2000), starting from
--   the genesis block. Blocks the thread while waiting for a response,
--   and in case of successful parse, verifies the validity of the
--   received chain.
requestAllBlockHeaders :: Socket -> ExceptT Text IO Network.HeadersMsgInfo
requestAllBlockHeaders s = do
  -- {{{
  Network.envelopeMessage True Network.getAllHeadersMsg
    & serialize
    & sendAll s
    & liftIO
  response <- liftIO $ recv s (2 ^ 32) -- this probably should be replaced with conduit.
  case parseHeaders response of
    Right (Network.Headers hInfo) ->
      -- {{{
      case Block.verifyChain (Network.hBlocks hInfo) of
        Right _ ->
          return hInfo
        Left err ->
          ExceptT $ return $ Left err
      -- }}}
    Left _ ->
      -- {{{
      ExceptT $ return $ Left "invalid headers response."
      -- }}}
  -- }}}


-- ** Utils
-- {{{
-- | Helper function to generate, envelope and send 
--   a @version@ message through its given socket.
sendVersion :: Socket -> IO ()
sendVersion s = do
  -- {{{
  verMsg <- Network.makeVersionMsg Nothing Nothing
  let bs = serialize $ Network.envelopeMessage True verMsg
  putStr "Sending: "
  print $ encodeHex bs
  sendAll s bs
  -- }}}


-- | Helper function to envelope and send the minimal
--   @verack@ message.
sendVerAck :: Socket -> IO ()
sendVerAck s = do
  -- {{{
  let bs = Network.VerAck Network.VerAckMsgInfo
           & Network.envelopeMessage True
           & serialize
  putStr "Sending: "
  print $ encodeHex bs
  sendAll s bs
  -- }}}


-- | Helper function.
parseVersion :: ByteString -> ParseResult Network.Message
parseVersion msg =
  -- {{{
  Network.Version <$> parse msg
  -- }}}


-- | Helper function.
parseVerAck :: ByteString -> ParseResult Network.Message
parseVerAck msg =
  -- {{{
  Network.VerAck <$> parse msg
  -- }}}


-- | Helper function.
parseHeaders :: ByteString -> ParseResult Network.Message
parseHeaders msg =
  -- {{{
  Network.Headers <$> parse msg
  -- }}}
-- }}}


