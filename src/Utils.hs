{-# LANGUAGE OverloadedStrings #-}


module Utils where


import           Debug.Trace             (trace)
import           Crypto.Hash             (hashWith, SHA256 (..), RIPEMD160 (..))
import qualified Data.Binary             as Bin
import           Data.Bits
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BAE
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Extension.ByteString    as BS
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Char               as Char
import           Data.Char               (chr)
import           Data.Function           ((&))
import           Data.Maybe              (fromMaybe)
import           Data.Memory.Endian      (getSystemEndianness, Endianness (..))
import           Data.String             (fromString)
import qualified Data.String             as String
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Word               as W


base58Chars :: [W.Word8]
base58Chars =
  -- {{{
  map chr [48..122]
  & filter
      ( \c ->
          Char.isAlphaNum c
          && c /= '0'
          && c /= 'O'
          && c /= 'l'
          && c /= 'I'
      )
  & fromString
  & BS.unpack
  -- }}}


encodeBase58 :: ByteString -> ByteString
encodeBase58 bs =
  -- {{{
  let
    (nulls, rest) = BS.partition (== 0) bs
    pre = BS.replicate (BS.length nulls) 49
  in
  pre <> integerToBase58 (bsToInteger rest)
  -- }}}

integerToBase58 :: Integer -> ByteString
integerToBase58 n =
  -- {{{
  let
    go theN soFar =
      -- {{{
      let
        (newN, mod) = divMod theN 58
      in
      if newN > 0 then
        go newN (fromInteger mod : soFar)
      else
        fromInteger mod : soFar
      -- }}}
  in
  go n []
  & map ((base58Chars !!) . fromIntegral)
  & BS.pack
  -- }}}


toBase58WithChecksum :: ByteString -> ByteString
toBase58WithChecksum bs =
  -- {{{
  let
    cs = BS.take 4 $ hash256 bs
    tier1 = bs <> cs
  in
  encodeBase58 tier1
  -- }}}


-- From the original "haskoin" project.
-- {{{
bsToInteger :: ByteString -> Integer
bsToInteger =
  -- {{{
  let
    f w n = toInteger w .|. shiftL n 8
  in
  BS.foldr f 0 . BS.invForBE
  -- }}}


integerToBS :: Integer -> ByteString
integerToBS i
  -- {{{
  | i > 0     =
      -- {{{
      let
        f 0 = Nothing
        f x = Just (fromInteger x :: W.Word8, x `shiftR` 8)
      in
      BS.invForBE $ BS.unfoldr f i
      -- }}}
  | otherwise =
      -- {{{
      BS.pack [0]
      -- }}}
  -- }}}


encodeHex :: ByteString -> ByteString
encodeHex = B16.encodeBase16'
-- }}}




integralToBS :: Integral n => n -> ByteString
integralToBS = integerToBS . toInteger


prependIntegerWithWord8 :: Maybe W.Word8 -> Integer -> ByteString
prependIntegerWithWord8 mW8 n =
  -- {{{
  maybe BS.empty (BS.pack . (: [])) mW8 <> integerToBS n
  -- }}}


appendWord8ToByteString :: W.Word8 -> ByteString -> ByteString
appendWord8ToByteString w8 bs =
  -- {{{
  Bin.encode w8
  & BSL.toStrict
  & flip BS.append bs
  -- }}}


hash160 :: ByteString -> ByteString
hash160 =
  -- {{{
  BA.convert . hashWith RIPEMD160 . hashWith SHA256
  -- }}}


hash256 :: ByteString -> ByteString
hash256 =
  -- {{{
  BA.convert . hashWith SHA256 . hashWith SHA256
  -- }}}




