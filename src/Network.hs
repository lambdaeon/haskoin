module Network where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import           Extension.ByteString.Parser
import           Text.Megaparsec             ((<|>))
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Byte        as BP
import           Utils


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




sampleEnvelopeBS :: ByteString
sampleEnvelopeBS = integerToBS 0xf9beb4d976657261636b000000000000000000005df6e0e2
