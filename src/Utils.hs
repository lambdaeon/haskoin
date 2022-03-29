{-# LANGUAGE OverloadedStrings #-}


-- MODULE
-- {{{
module Utils
  ( encodeHex
  , encodeHexLE
  , encodeBase58
  , decodeHex
  , decodeHexLE
  -- , decodeBase58
  , base58Chars
  , getIndexOfBase58Char
  -- 
  , showIntegralInBase58
  , integerToBase58
  , base58ToInteger
  , toBase58WithChecksum
  , showBase58EncodedBS
  , bsToInteger
  , bsToIntegerLE
  , bsToSignedIntegral
  , bsToSignedIntegralLE
  , integerToBS
  , integerToBSLE
  , integralToBS
  , integralToBSLE
  , signedIntegralToBS
  , signedIntegralToBSLE
  , integralToNBytes
  , integralToNBytesLE
  , integralTo32Bytes
  , integralTo32BytesLE
  , base16StringToBS
  , prependIntegerWithWord8
  , sha1
  , sha256
  , ripemd160
  , hash160
  , hash256
  , indexedMap
  , indexedMapM
  , eitherToMaybe
  , (!?)
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Debug.Trace                 (trace)
import           Control.Monad               (zipWithM)
import           Crypto.Hash                 (hashWith, SHA1 (..), SHA256 (..), RIPEMD160 (..))
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
import           Data.List                   (foldl')
import           Data.Function               ((&))
import           Data.Maybe                  (fromMaybe)
import           Data.Memory.Endian          (getSystemEndianness, Endianness (..))
import           Data.String                 (fromString)
import qualified Data.String                 as String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Word                   (Word8, Word32)
import qualified Extension.ByteString        as BS
import qualified Extension.ByteString.Lazy   as LBS
import           Numeric                     (showIntAtBase)
-- }}}


-- FUNCTIONS
-- {{{
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


getIndexOfBase58Char :: Word8 -> Maybe Integer
getIndexOfBase58Char target =
  -- {{{
  let
    go _          []                = Nothing
    go Nothing    _                 = Nothing
    go (Just acc) (currByte : rest) =
      if currByte == target then
        Just acc
      else
        go (Just $ acc + 1) rest
  in
  go (Just 0) base58Chars
  -- }}}


encodeBase58 :: ByteString -> ByteString
encodeBase58 bs =
  -- {{{
  let
    (nulls, rest) = LBS.break (/= 0) bs
    -- pre = LBS.replicate (LBS.length nulls) 49 -- '1' is 0 in Base58
    pre = LBS.replicate (LBS.length nulls) 0
  in
  pre <> integerToBase58 (bsToInteger rest)
  -- }}}


base58ToInteger :: ByteString -> Maybe Integer
base58ToInteger bs =
  -- {{{
  let
    foldFn _        Nothing             = Nothing
    foldFn currByte (Just (count, acc)) =
      Just
        ( count + 1
        , fromIntegral currByte * 58 ^ count + acc
        )
  in
  snd <$> LBS.foldr foldFn (Just (0, 0)) bs
  -- }}}


showIntegralInBase58 :: (Integral a, Show a) => a -> Maybe String
showIntegralInBase58 x
  -- {{{
  | x < 0     = Nothing
  | otherwise =
      Just $ showIntAtBase
        58
        (\ind -> chr $ fromIntegral (base58Chars !! ind))
        x
        ""
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
  -- & map ((base58Chars !!) . fromIntegral)
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


showBase58EncodedBS :: ByteString -> Maybe String
showBase58EncodedBS bs =
    LBS.unpack bs
  & traverse ((chr . fromIntegral <$>) . (base58Chars !?) . fromIntegral)


-- From the original "haskoin" project.
---------------------------------------
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


bsToSignedIntegralHelper :: Integral a => Bool -> ByteString -> a
bsToSignedIntegralHelper be bs
  -- {{{
  | LBS.null bs = 0
  | otherwise   =
      let
        (tier1, revIfLE, fn) =
          -- {{{
          if be then
            -- {{{
            (bs, id, fromInteger . bsToInteger)
            -- }}}
          else
            -- {{{
            (LBS.reverse bs, LBS.reverse, fromInteger . bsToIntegerLE)
            -- }}}
          -- }}}
        (bytes, negMult) =
          -- {{{
          case LBS.unpack tier1 of
            fstByte : rest ->
              -- {{{
              if fstByte .&. 0x80 == 0x80 then
                ( revIfLE $ LBS.pack $ (fstByte .&. 0x7f) : rest
                , ((-1) *)
                )
              else
                (revIfLE bs, id)
              -- }}}
            _ ->
              -- {{{
              (revIfLE bs, id)
              -- }}}
          -- }}}
      in
      negMult $ fn bytes
  -- }}}
bsToSignedIntegral :: Integral a => ByteString -> a
bsToSignedIntegral   = bsToSignedIntegralHelper True
bsToSignedIntegralLE :: Integral a => ByteString -> a
bsToSignedIntegralLE = bsToSignedIntegralHelper False


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


signedIntegralToBSHelper :: Integral a => Bool -> a -> ByteString
signedIntegralToBSHelper be i_
  -- {{{
  | i_ == 0   = LBS.empty
  | otherwise =
      let
        i     = abs i_
        isNeg = i_ < 0
        (tier1, addAsBiggestByte, getBiggestByte, switchOnBiggestByte) =
          if be then
            -- {{{
            ( integralToBS i
            , LBS.cons'
            , LBS.head
            , \bs ->
                case LBS.unpack bs of
                  biggest : rest -> LBS.pack $ (biggest .|. 0x80) : rest
                  _              -> bs
            )
            -- }}}
          else
            -- {{{
            ( integralToBSLE i
            , flip LBS.snoc
            , LBS.last
            , \bs ->
                case LBS.unpack (LBS.reverse bs) of
                  biggest : rest -> LBS.reverse $ LBS.pack $ (biggest .|. 0x80) : rest
                  _              -> bs
            )
            -- }}}
      in
      if (getBiggestByte tier1 .&. 0x80) == 0x80 then
        -- {{{
        if isNeg then
          0x80 `addAsBiggestByte` tier1
        else
          0x00 `addAsBiggestByte` tier1
        -- }}}
      else if isNeg then
        -- {{{
        switchOnBiggestByte tier1
        -- }}}
      else
        -- {{{
        tier1
        -- }}}
  -- }}}


signedIntegralToBS :: Integral a => a -> ByteString
signedIntegralToBS = signedIntegralToBSHelper True


signedIntegralToBSLE :: Integral a => a -> ByteString
signedIntegralToBSLE = signedIntegralToBSHelper False


encodeHex :: ByteString -> ByteString
encodeHex = B16.encodeBase16'

encodeHexLE :: ByteString -> ByteString
encodeHexLE = encodeHex . LBS.invForLE
---------------------------------------


decodeHex :: ByteString -> Either Text ByteString
decodeHex = B16.decodeBase16


decodeHexLE :: ByteString -> Either Text ByteString
decodeHexLE = decodeHex . LBS.invForLE



integralToBS :: Integral n => n -> ByteString
integralToBS = integerToBS . toInteger

integralToBSLE :: Integral n => n -> ByteString
integralToBSLE = integerToBSLE . toInteger


integralToNBytesHelper :: Integral a => Bool -> Word -> a -> ByteString
integralToNBytesHelper be n_ x =
  -- {{{
  let
    n = fromIntegral n_
    (tier1, op) =
      if be then
        (integralToBS   x, (<>)     )
      else
        (integralToBSLE x, flip (<>))
  in
  LBS.replicate (n - min n (fromIntegral $ LBS.length tier1)) 0x00 `op` tier1
  -- }}}


integralToNBytes :: Integral n => Word -> n -> ByteString
integralToNBytes =
  -- {{{
  integralToNBytesHelper True
  -- }}}


integralToNBytesLE :: Integral n => Word -> n -> ByteString
integralToNBytesLE =
  -- {{{
  integralToNBytesHelper False
  -- }}}


integralTo32Bytes :: Integral n => n -> ByteString
integralTo32Bytes =
  -- {{{
  integralToNBytes 32
  -- }}}


integralTo32BytesLE :: Integral n => n -> ByteString
integralTo32BytesLE =
  -- {{{
  integralToNBytesLE 32
  -- }}}


base16StringToBS :: String -> Maybe ByteString
base16StringToBS b16 =
  -- {{{
  case B16.decodeBase16 (fromString b16) of
    Right bs ->
      Just bs
    _ ->
      Nothing
  -- }}}


prependIntegerWithWord8 :: Maybe Word8 -> Integer -> ByteString
prependIntegerWithWord8 mW8 n =
  -- {{{
  maybe LBS.empty LBS.singleton mW8 <> integerToBS n
  -- }}}


sha1 :: ByteString -> ByteString
sha1 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA1 . LBS.toStrict
  -- }}}


sha256 :: ByteString -> ByteString
sha256 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA256 . LBS.toStrict
  -- }}}

ripemd160 :: ByteString -> ByteString
ripemd160 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith RIPEMD160 . LBS.toStrict
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


indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
  -- {{{
  let
    go _     soFar []       = soFar
    go index soFar (y : ys) = go (index + 1) (f index y : soFar) ys
  in
  reverse $ go 0 [] xs
  -- }}}

indexedMapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
indexedMapM fn xs = zipWithM fn [0 .. length xs - 1] xs


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing


-- from `extra` package.
(!?) :: [a] -> Int -> Maybe a
xs !? n
  -- {{{
  | n < 0     =
      Nothing
  | otherwise =
      foldr
        ( \x r k ->
            case k of
              0 ->
                Just x
              _ ->
                r (k-1)
        )
        (const Nothing)
        xs
        n
  -- }}}

-- }}}



