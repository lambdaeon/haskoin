module Network.Common where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import           Extension.ByteString.Parser
import qualified Network.Socket              as Socket
import qualified Text.Megaparsec             as P
import           Utils


data IP
  = IPv4 Word8 Word8 Word8 Word8
  | IPv6 Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16
  deriving (Eq, Show)


instance Serializable IP where
  serialize (IPv4 b0 b1 b2 b3) =
    -- {{{
       LBS.replicate 10 0x00
    <> LBS.replicate 2  0xff
    <> LBS.pack [b0, b1, b2, b3]
    -- }}}
  serialize (IPv6 bs0 bs1 bs2 bs3 bs4 bs5 bs6 bs7) =
    -- {{{
        serialize
    <$> [bs0, bs1, bs2, bs3, bs4, bs5, bs6, bs7]
    & LBS.concat
    -- }}}
  parser = do
    -- {{{
    bs0 <- parser
    bs1 <- parser
    bs2 <- parser
    bs3 <- parser
    bs4 <- parser
    bs5 <- parser
    bs6 <- parser
    -- bs7 <- parser
    -- return $ toIPv4Attempt $ IPv6 bs0 bs1 bs2 bs3 bs4 bs5 bs6 bs7
    toIPv4Attempt . IPv6 bs0 bs1 bs2 bs3 bs4 bs5 bs6 <$> parser
    -- }}}


toIPv4Attempt :: IP -> IP
toIPv4Attempt ip@IPv4 {} = ip
toIPv4Attempt ip@(IPv6 bs0 bs1 bs2 bs3 bs4 bs5 bs6 bs7)
  -- {{{
  |    bs0 == 0x0000
    && bs1 == 0x0000
    && bs2 == 0x0000
    && bs3 == 0x0000
    && bs4 == 0x0000
    && bs4 == 0xffff     =
      case LBS.unpack (serialize bs6 <> serialize bs7) of
        [b0, b1, b2, b3] ->
          IPv4 b0 b1 b2 b3
        _ ->
          ip
  | otherwise            = ip
  -- }}}


