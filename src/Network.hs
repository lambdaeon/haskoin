module Network where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import           Data.Serializable
import           Extension.ByteString.Parser
import           Network.Common
import           Text.Megaparsec             ((<|>))
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Byte        as BP
import           Utils


-- ENVELOPE
-- {{{
data Envelope = Envelope
  { envCommand :: ByteString
  , envPayload :: ByteString
  , envTestnet :: Bool
  } deriving (Eq)

instance Show Envelope where
  -- {{{
  show Envelope {..} =
    let
      showLBS bs
        | LBS.null bs = "<EMPTY BYTESTRING>"
        | otherwise   =
            chr . fromIntegral <$> LBS.unpack bs
    in
       "NetworkEnvelop {envCommand = "
    ++ showLBS envCommand
    ++ ", envPayload = "
    ++ showLBS envPayload
    ++ ", envTestnet = "
    ++ show envTestnet
    ++ "}"
  -- }}}

instance Serializable Envelope where
  serialize Envelope {..} =
    -- {{{
       (if envTestnet then testnetNetworkMagic else mainnetNetworkMagic)
    <> envCommand
    <> integralToNBytesLE 4 (LBS.length envPayload)
    <> LBS.take 4 (hash256 envPayload)
    <> envPayload
    -- }}}
  parser = do
    -- {{{
    magic <- BP.string mainnetNetworkMagic <|> BP.string testnetNetworkMagic
    let envTestnet = magic == testnetNetworkMagic
    envCommand       <- P.takeP (Just "network command.") 12
    payloadLength    <- fromIntegral <$> word32ParserLE "network payload length."
    payloadChecksum  <- P.takeP (Just "network payload checksum.") 4
    envPayload       <- P.takeP (Just "network payload.") payloadLength
    if payloadChecksum == LBS.take 4 (hash256 envPayload) then
      return $ Envelope {..}
    else
      fail "payload checksum failed."
    -- }}}
-- }}}


-- MESSAGE
-- {{{
data Message
  = Version VersionMsgInfo
  | VerAck  VerAckMsgInfo


-- VersionMsgInfo
-- {{{
data VersionMsgInfo = VersionMsgInfo
  { verProtocolVersion  :: Word32
  , verServices         :: Word64
  , verTimestamp        :: Word64
  , verReceiverServices :: Word64
  , verReceiverIP       :: IP
  , verReceiverPort     :: Word16
  , verSenderServices   :: Word64
  , verSenderIP         :: IP
  , verSenderPort       :: Word16
  , verNonce            :: Word64
  , verUserAgent        :: ByteString
  , verLatestBlock      :: Word64
  , verRelay            :: Bool
  } deriving (Eq, Show)

instance Serializable VersionMsgInfo where
  serialize VersionMsgInfo {..} =
    -- {{{
       serialize verProtocolVersion
    <> serialize verServices
    <> serialize verTimestamp
    <> serialize verReceiverServices
    <> serialize verReceiverIP
    <> serialize verReceiverPort
    <> serialize verSenderServices
    <> serialize verSenderIP
    <> serialize verSenderPort
    <> serialize verNonce
    <> serialize (Varint $ fromIntegral $ LBS.length verUserAgent)
    <> verUserAgent
    <> serialize verLatestBlock
    <> serialize verRelay
    -- }}}
  parser = do    
    -- {{{
    verProtocolVersion  <- parser
    verServices         <- parser
    verTimestamp        <- parser
    verReceiverServices <- parser
    verReceiverIP       <- parser
    verReceiverPort     <- parser
    verSenderServices   <- parser
    verSenderIP         <- parser
    verSenderPort       <- parser
    verNonce            <- parser
    uaLen               <- Varint.countParser
    verUserAgent        <- P.takeP (Just "network version message user agent.") uaLen
    verLatestBlock      <- parser
    verRelay            <- parser
    return $ VersionMsgInfo {..}
    -- }}}

makeVersionMsg :: Maybe Word64 -> Maybe Word64 -> ExceptT Text IO Message
makeVersionMsg mTS mNonce = do
  -- {{{
  verTimestamp <- case mTS of
                    Just ts    -> return ts
                    Nothing    -> liftIO getPOSIX
  verNonce     <- case mNonce of
                    Just nonce -> return nonce
                    Nothing    -> liftIO $ getNByteNonce 8
  let verProtocolVersion  = 70015
      verServices         = 0
      verReceiverServices = 0
      verReceiverIP       = IPv4 0 0 0 0
      verReceiverPort     = 8333
      verSenderServices   = 0
      verSenderIP         = IPv4 0 0 0 0
      verSenderPort       = 8333
      verUserAgent        = "programmingbitcoin:0.1"
      verLatestBlock      = 0
      verRelay            = False
  return $ Version $ VersionMsgInfo {..}
  -- }}}
-- }}}


-- VerAckMsgInfo
-- {{{
data VerAckMsgInfo = VerAckMsgInfo

instance Serializable VerAckMsgInfo where
  serialize _ = "verack"
  parser      = VerAckMsgInfo <$ BP.string "verack"
-- }}}


serializeMessage :: Message -> ByteString
serializeMessage (Version verMsgInfo) = serialize verMsgInfo
serializeMessage (VerAck  verAckInfo) = serialize verAckInfo
-- }}}



sampleEnvelopeBS :: ByteString
sampleEnvelopeBS = integerToBS 0xf9beb4d976657261636b000000000000000000005df6e0e2
