module Extension.ByteString.Lazy
  ( invForBE
  , invForLE
  , chunksOf
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
