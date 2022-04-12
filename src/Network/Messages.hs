module Network.Messages where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import qualified Data.Varint                 as Varint
import           Data.Varint                 (Varint (..))
import           Extension.ByteString.Parser
import           Network.Common
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Byte        as BP
import           Utils


data Version = Version
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

instance Serializable Version where
  serialize Version {..} =
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
    return $ Version {..}
    -- }}}


initVersion :: Maybe Word64 -> Maybe Word64 -> ExceptT Text IO Version
initVersion mTS mNonce = do
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
  return $ Version {..}
  -- }}}



data VerAck = VerAck

instance Serializable VerAck where
  serialize _ = "verack"
  parser      = VerAck <$ BP.string "verack"







