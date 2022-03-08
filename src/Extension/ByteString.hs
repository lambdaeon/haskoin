module Extension.ByteString
  ( invForBE
  , invForLE
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


invForLE :: ByteString -> ByteString
invForLE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      id
    BigEndian ->
      BS.reverse
  -- }}}
