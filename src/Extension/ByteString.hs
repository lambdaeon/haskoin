module Extension.ByteString
  ( invForBE
  ) where


import Data.Memory.Endian (getSystemEndianness, Endianness (..))
import Data.ByteString as BS


invForBE :: ByteString -> ByteString
invForBE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      BS.reverse
    BigEndian ->
      id
  -- }}}
