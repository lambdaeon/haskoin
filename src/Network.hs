module Network where


import           BlockHead                   (BlockHead)
import           Control.Monad               (replicateM)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import           Data.Serializable
import           Extension.ByteString.Parser
import qualified Extension.ByteString.Lazy   as LBS
import           Network.Common
import           Text.Megaparsec             ((<|>))
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Byte        as BP
import           Utils


-- ** Envelope
-- {{{
-- | Recode type to represent the envelope that contains the
--   actual payload.
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
            chr . fromIntegral <$> LBS.unpack (encodeHex bs)
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
    <> LBS.appendZerosUntil 12 envCommand
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


-- | Smart constructor of `Evelope` values based on the given
--   `Message` data constructor.
envelopeMessage :: Bool -> Message -> Envelope
envelopeMessage testnet (Version info) =
  -- {{{
  Envelope
    { envCommand = "version"
    , envPayload = serialize info
    , envTestnet = testnet
    }
  -- }}}
envelopeMessage testnet (VerAck info) =
  -- {{{
  Envelope
    { envCommand = "verack"
    , envPayload = serialize info
    , envTestnet = testnet
    }
  -- }}}
envelopeMessage testnet (GetHeaders info) =
  -- {{{
  Envelope
    { envCommand = "getheaders"
    , envPayload = serialize info
    , envTestnet = testnet
    }
  -- }}}
envelopeMessage testnet (Headers info) =
  -- {{{
  Envelope
    { envCommand = "headers"
    , envPayload = serialize info
    , envTestnet = testnet
    }
  -- }}}
-- }}}


-- ** Message
-- {{{
-- | Sum type to allow a more concise representation of
--   various messages.
data Message
  = Version    VersionMsgInfo
  | VerAck     VerAckMsgInfo
  | GetHeaders GetHeadersMsgInfo
  | Headers    HeadersMsgInfo
  deriving (Eq, Show)


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
  , verLatestBlock      :: Word32
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
    <> LBS.reverse (serialize verReceiverPort)
    <> serialize verSenderServices
    <> serialize verSenderIP
    <> LBS.reverse (serialize verSenderPort)
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


-- | Function for generating a `Version` message with many of its
--   fields set to certain default values (many of which are probably
--   not quite correct).
makeVersionMsg :: Maybe Word64 -> Maybe Word64 -> IO Message
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
      verUserAgent        = "/programmingbitcoin:0.1/"
      verLatestBlock      = 0
      verRelay            = False
  return $ Version $ VersionMsgInfo {..}
  -- }}}
-- }}}


-- VerAckMsgInfo
-- {{{
-- | A minimal datatype to represent a @verack@ message.
data VerAckMsgInfo = VerAckMsgInfo deriving (Eq, Show)

instance Serializable VerAckMsgInfo where
  serialize _ = LBS.empty
  parser      = return VerAckMsgInfo
-- }}}


-- GetHeadersMsgInfo
-- {{{
data GetHeadersMsgInfo = GetHeadersMsgInfo
  { ghVersion       :: Word32
  , ghNumHashes     :: Varint
  , ghStartingBlock :: ByteString
  , ghEndingBlock   :: ByteString
  } deriving (Eq, Show)

instance Serializable GetHeadersMsgInfo where
  serialize GetHeadersMsgInfo {..} =
    -- {{{
       serialize ghVersion
    <> serialize ghNumHashes
    <> LBS.appendZerosUntil 32 ghStartingBlock
    <> ( if LBS.null ghEndingBlock then
           LBS.appendZerosUntil 32 LBS.empty
         else
           LBS.appendZerosUntil 32 ghEndingBlock
       )
    -- }}}
  parser = do
    -- {{{
    ghVersion       <- parser
    ghNumHashes     <- parser
    ghStartingBlock <- P.takeP (Just "GetHeaders starting block.") 32
    ghEndingBlock   <- P.takeP (Just "GetHeaders ending block.")   32
    return $ GetHeadersMsgInfo {..}
    -- }}}
-- }}}


-- HeadersMsgInfo
-- {{{
data HeadersMsgInfo = HeadersMsgInfo
  { hBlocks :: [BlockHead]
  } deriving (Eq, Show)

instance Serializable HeadersMsgInfo where
  serialize HeadersMsgInfo {..} =
    -- {{{
    let
      varintCount = Varint $ fromIntegral $ length hBlocks
      foldFn bh acc = acc <> (serialize bh) `LBS.snoc` 0x00
    in
       serialize varintCount
    <> foldr foldFn LBS.empty hBlocks
    -- }}}
  parser =
    -- {{{
    let
      customParser :: Parser BlockHead
      customParser = do
        block <- parser
        P.anySingle
        return block
    in do
    blocksCount <- Varint.countParser
    hBlocks     <- replicateM blocksCount customParser
    return $ HeadersMsgInfo {..}
    -- }}}
-- }}}



-- | With the current architecture of `Serializable` typeclass,
--   implementing a `parser` for the `Message` datatype seems a bit
--   of a hassle. To help accelerate the progress, I've implemented
--   its serialization function seprarately here.
serializeMessage :: Message -> ByteString
serializeMessage (Version    verMsgInfo) = serialize verMsgInfo
serializeMessage (VerAck     verAckInfo) = serialize verAckInfo
serializeMessage (GetHeaders ghInfo    ) = serialize ghInfo
serializeMessage (Headers    hInfo     ) = serialize hInfo
-- }}}



sampleEnvelopeBS :: ByteString
sampleEnvelopeBS = integerToBS 0xf9beb4d976657261636b000000000000000000005df6e0e2
