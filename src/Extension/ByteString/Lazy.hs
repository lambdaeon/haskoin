module Extension.ByteString.Lazy
  ( invForBE
  , invForLE
  ) where


import Data.Memory.Endian (getSystemEndianness, Endianness (..))
import Data.ByteString.Lazy as LBS


invForBE :: ByteString -> ByteString
invForBE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      LBS.reverse
    BigEndian ->
      id
  -- }}}


invForLE :: ByteString -> ByteString
invForLE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      id
    BigEndian ->
      LBS.reverse
  -- }}}
