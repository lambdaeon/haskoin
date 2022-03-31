module Extension.ByteString
  ( invForBE
  , invForLE
  ) where


import Data.Memory.Endian (getSystemEndianness, Endianness (..))
import Data.ByteString as BS


-- | Function which aims to result in a big-endian
--   `ByteString`, based on the endianness of the system.
invForBE :: ByteString -> ByteString
invForBE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      BS.reverse
    BigEndian ->
      id
  -- }}}


-- | Function which aims to result in a little-endian
--   `ByteString`, based on the endianness of the system.
invForLE :: ByteString -> ByteString
invForLE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      id
    BigEndian ->
      BS.reverse
  -- }}}
