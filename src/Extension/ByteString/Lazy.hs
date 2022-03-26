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


build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []


-- from the `split` package.
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


safeLast :: ByteString -> Maybe Word8
safeLast bs =
  -- {{{
  if LBS.length bs <= 0 then
    Nothing
  else
    Just $ LBS.last bs
  -- }}}


safeInit :: ByteString -> Maybe ByteString
safeInit bs =
  -- {{{
  if LBS.length bs <= 0 then
    Nothing
  else
    Just $ LBS.init bs
  -- }}}



