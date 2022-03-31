module Extension.ByteString.Lazy
  ( invForBE
  , invForLE
  , chunksOf
  , safeLast
  , safeInit
  ) where


import Data.ByteString.Lazy as LBS
import Data.Memory.Endian      (getSystemEndianness, Endianness (..))
import Data.Word               (Word8)



-- | Function which aims to result in a big-endian
--   `ByteString`, based on the endianness of the system.
invForBE :: ByteString -> ByteString
invForBE =
  -- {{{
  case getSystemEndianness of
    LittleEndian ->
      LBS.reverse
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
      LBS.reverse
  -- }}}


-- | Helper function used in `chunksOf`
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []


-- | From the @split@ package, a function that turns a `ByteString`
--   into a list of `ByteString` values with specific byte counts.
chunksOf :: Int -> ByteString -> [ByteString]
chunksOf i bs =
  -- {{{
  let
    ls = LBS.unpack bs
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (Prelude.drop i l) c n
  in
  fmap (LBS.pack . Prelude.take i) (build (splitter ls))
  -- }}}


-- | Safe version of the original @last@ function.
safeLast :: ByteString -> Maybe Word8
safeLast bs =
  -- {{{
  if LBS.length bs <= 0 then
    Nothing
  else
    Just $ LBS.last bs
  -- }}}


-- | Safe version of the original @init@ function
--   (which drops the last byte).
safeInit :: ByteString -> Maybe ByteString
safeInit bs =
  -- {{{
  if LBS.length bs <= 0 then
    Nothing
  else
    Just $ LBS.init bs
  -- }}}



