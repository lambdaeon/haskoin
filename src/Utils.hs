{-# LANGUAGE OverloadedStrings #-}


module Utils
  ( encodeHex
  , encodeHexLE
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


import           Debug.Trace                 (trace)
import           Crypto.Hash                 (hashWith, SHA256 (..), RIPEMD160 (..))
import qualified Data.Binary                 as Bin
import           Data.Bits
import qualified Data.ByteArray              as BA
import qualified Data.ByteArray.Encoding     as BAE
import qualified Data.ByteString             as BS
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Base16 as B16
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Char                   as Char
import           Data.Char                   (chr)
import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import           Data.Memory.Endian          (getSystemEndianness, Endianness (..))
import           Data.String                 (fromString)
import qualified Data.String                 as String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Word                   (Word8)
import           Data.Void
import qualified Extension.ByteString        as BS
import qualified Extension.ByteString.Lazy   as LBS
import           Text.Megaparsec       (Parsec)
import qualified Text.Megaparsec       as P
import qualified Text.Megaparsec.Byte  as BP


type Parser = Parsec Void ByteString


base58Chars :: [Word8]
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
  & LBS.unpack
  -- }}}


encodeBase58 :: ByteString -> ByteString
encodeBase58 bs =
  -- {{{
  let
    (nulls, rest) = LBS.break (/= 0) bs
    pre = LBS.replicate (LBS.length nulls) 49 -- '1' is 0 in Base58
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
  & LBS.pack
  -- }}}


toBase58WithChecksum :: ByteString -> ByteString
toBase58WithChecksum bs =
  -- {{{
  let
    cs = LBS.take 4 $ hash256 bs
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
  LBS.foldr f 0 . (if be then LBS.invForBE else LBS.invForLE)
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
        f x = Just (fromInteger x :: Word8, x `shiftR` 8)
      in
      (if be then LBS.invForBE else LBS.invForLE) $ LBS.unfoldr f i
      -- }}}
  | otherwise =
      -- {{{
      LBS.pack [0]
      -- }}}
  -- }}}


encodeHex :: ByteString -> ByteString
encodeHex = B16.encodeBase16'

encodeHexLE :: ByteString -> ByteString
encodeHexLE = encodeHex . LBS.invForLE
-- }}}


integralToBS :: Integral n => n -> ByteString
integralToBS = integerToBS . toInteger

integralToBSLE :: Integral n => n -> ByteString
integralToBSLE = integerToBSLE . toInteger


integralTo32BytesHelper :: Integral n => Bool -> n -> ByteString
integralTo32BytesHelper be n =
  -- {{{
  let
    (tier1, op) =
      if be then
        (integralToBS   n, (<>)     )
      else
        (integralToBSLE n, flip (<>))
  in
  LBS.replicate (32 - min 32 $ LBS.length tier1) 0x00 `op` tier1
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


prependIntegerWithWord8 :: Maybe Word8 -> Integer -> ByteString
prependIntegerWithWord8 mW8 n =
  -- {{{
  maybe LBS.empty (LBS.pack . (: [])) mW8 <> integerToBS n
  -- }}}


hash160 :: ByteString -> ByteString
hash160 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith RIPEMD160 . hashWith SHA256 . LBS.toStrict
  -- }}}


hash256 :: ByteString -> ByteString
hash256 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA256 . hashWith SHA256 . LBS.toStrict
  -- }}}




