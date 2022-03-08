{-# LANGUAGE OverloadedStrings #-}


module Utils
  ( encodeHex
  , encodeBase58
  , integerToBase58
  , toBase58WithChecksum
  , bsToInteger
  , bsToIntegerLE
  , integerToBS
  , integerToBSLE
  , integralToBS
  , integralToBSLE
  , integralTo32Bytes
  , integralTo32BytesLE
  , prependIntegerWithWord8
  , hash160
  , hash256
  ) where


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
    (nulls, rest) = BS.break (/= 0) bs
    pre = BS.replicate (BS.length nulls) 49 -- '1' is 0 in Base58
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
  bsToIntegerHelper True
  -- }}}


bsToIntegerLE :: ByteString -> Integer
bsToIntegerLE =
  -- {{{
  bsToIntegerHelper False
  -- }}}


bsToIntegerHelper :: Bool -> ByteString -> Integer
bsToIntegerHelper be =
  -- {{{
  let
    f w n = toInteger w .|. shiftL n 8
  in
  BS.foldr f 0 . (if be then BS.invForBE else BS.invForLE)
  -- }}}


integerToBS :: Integer -> ByteString
integerToBS =
  -- {{{
  integerToBSHelper True
  -- }}}


integerToBSLE :: Integer -> ByteString
integerToBSLE =
  -- {{{
  integerToBSHelper False
  -- }}}


integerToBSHelper :: Bool -> Integer -> ByteString
integerToBSHelper be i
  -- {{{
  | i > 0     =
      -- {{{
      let
        f 0 = Nothing
        f x = Just (fromInteger x :: W.Word8, x `shiftR` 8)
      in
      (if be then BS.invForBE else BS.invForLE) $ BS.unfoldr f i
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

integralToBSLE :: Integral n => n -> ByteString
integralToBSLE = integerToBSLE . toInteger


integralTo32BytesHelper :: Integral n => Bool -> n -> ByteString
integralTo32BytesHelper be n =
  -- {{{
  let
    tier1 = (if be then integralToBS else integralToBSLE) n
  in
  BS.replicate (32 - BS.length tier1) 0 <> tier1
  -- }}}


integralTo32Bytes :: Integral n => n -> ByteString
integralTo32Bytes =
  -- {{{
  integralTo32BytesHelper True
  -- }}}


integralTo32BytesLE :: Integral n => n -> ByteString
integralTo32BytesLE =
  -- {{{
  integralTo32BytesHelper False
  -- }}}


prependIntegerWithWord8 :: Maybe W.Word8 -> Integer -> ByteString
prependIntegerWithWord8 mW8 n =
  -- {{{
  maybe BS.empty (BS.pack . (: [])) mW8 <> integerToBS n
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




