module Extension.ByteString.Lazy
  ( invForBE
  , invForLE
  , chunksOf
  , safeLast
  , safeInit
  , dropEnd
  , appendZerosUntil
  ) where


import Data.ByteString.Lazy as LBS
import Data.Memory.Endian      (getSystemEndianness, Endianness (..))
import Data.Word               (Word8)
import GHC.Int                 (Int64)



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



dropEnd :: Int -> ByteString -> Maybe ByteString
dropEnd i' bs =
  let
    i   = fromIntegral i'
    len = LBS.length bs
  in
  if i > LBS.length bs then
    Nothing
  else
    Just $ LBS.take (len - i) bs


-- | Zero padding to the right until the given `ByteString`
--   reaches the desired length. Doesn't do anything if the
--   given `ByteString` is longer.
appendZerosUntil :: Int64 -> ByteString -> ByteString
appendZerosUntil maxLen bs =
  bs <> LBS.replicate (maxLen - LBS.length bs) 0x00





















