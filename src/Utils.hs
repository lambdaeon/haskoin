-- MODULE
-- {{{
module Utils
  ( threeAndAHalfDays
  , twoWeeks
  , eightWeeks
  , mainnetNetworkMagic
  , testnetNetworkMagic
  , encodeHex
  , encodeHexLE
  , encodeBase58
  , decodeHex
  , decodeHexLE
  , decodeBase58
  , decodeBase58WithChecksum
  , showIntegralInBase58
  , base58Chars
  , getIndexOfBase58Char
  , integerToBase58
  , base58ToInteger
  , base58StringToBS
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
  , word8ToBools
  , boolsToWord8
  , splitIn
  , indexedMap
  , indexedMapM
  , foldM
  , eitherToMaybe
  , (!?)
  , hoistMaybe
  , fromTwoEitherValues
  , explainMaybe
  , mapLeft
  , getNByteNonce
  , getPOSIX
  , clamp
  , trace
  , myTrace
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans.Except
  , ByteString
  , chr
  , foldl'
  , (&)
  , isJust
  , isNothing
  , fromMaybe
  , fromString
  , Text
  , Word8
  , Word16
  , Word32
  , Word64
  , Word
  , module Data.Void
  , void
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Debug.Trace                 (trace)
import           Control.Monad               (zipWithM, foldM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Crypto.Hash                 (hashWith, SHA1 (..), SHA256 (..), RIPEMD160 (..))
import           Crypto.Random               (getRandomBytes)
import qualified Data.Binary                 as Bin
import           Data.Bits
import qualified Data.ByteArray              as BA
import qualified Data.ByteArray.Encoding     as BAE
import qualified Data.ByteString             as BS
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy.Base16 as B16
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Char                   as Char
import           Data.Char                   (chr, ord)
import           Data.List                   (foldl')
import           Data.Function               ((&))
import           Data.Functor                (void)
import           Data.Maybe                  (isJust, isNothing, fromMaybe)
import           Data.Memory.Endian          (getSystemEndianness, Endianness (..))
import           Data.String                 (fromString)
import qualified Data.String                 as String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Void
import           Data.Word                   (Word8, Word16, Word32, Word64, Word)
import qualified Extension.ByteString        as BS
import qualified Extension.ByteString.Lazy   as LBS
import           Numeric                     (showIntAtBase)
-- }}}


-- CONSTANTS
-- {{{
threeAndAHalfDays :: Num a => a
threeAndAHalfDays = 60 * 60 * (24 * 3 + 12)


twoWeeks :: Num a => a
twoWeeks = threeAndAHalfDays * 4


eightWeeks :: Num a => a
eightWeeks = twoWeeks * 4


mainnetNetworkMagic :: ByteString
mainnetNetworkMagic = integralToNBytes 4 0xf9beb4d9


testnetNetworkMagic :: ByteString
testnetNetworkMagic = integralToNBytes 4 0x0b110907
-- }}}


-- FUNCTIONS
-- {{{
-- | List of valid characters for Base58.
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


getIndexOfBase58Char :: Word8 -> Either Text Integer
getIndexOfBase58Char target =
  -- {{{
  let
    go _          []                = Left "byte out of bounds for base58."
    go (Left err)    _              = Left err
    go (Right acc) (currByte : rest) =
      if currByte == target then
        Right acc
      else
        go (Right $ acc + 1) rest
  in
  go (Right 0) base58Chars
  -- }}}


-- | Converts a `String` to a Base58 lazy `ByteString`.
base58StringToBS :: String -> Either Text ByteString
base58StringToBS str =
  -- {{{
  LBS.pack <$>
    traverse
      (   (fromIntegral <$>)
        . getIndexOfBase58Char
        . fromIntegral
        . ord
      )
      str
  -- decodeBase58 initBS
  -- }}}


-- | Encodes a `ByteString` into Base58. Preserves @0@ padding.
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


-- | Decoding from a Base58 encoded `ByteString`. Fails if
--   bytes bigger than @0x39@ (@57@) are encountered.
decodeBase58 :: ByteString -> Either Text ByteString
decodeBase58 bs = do
  -- {{{
  num <- base58ToInteger bs
  return $ integerToBS num
  -- }}}


-- | Converts a `ByteString` into an integer. Fails if
--   bytes bigger than @0x39@ (@57@) are encountered.
base58ToInteger :: ByteString -> Either Text Integer
base58ToInteger bs =
  -- {{{
  let
    foldFn _        err@(Left _)         = err
    foldFn currByte (Right (count, acc)) =
      if currByte > 0x39 then
        Left "bad base58."
      else
        Right
          ( count + 1
          , fromIntegral currByte * 58 ^ count + acc
          )
  in
  snd <$> LBS.foldr foldFn (Right (0, 0)) bs
  -- }}}


-- | Pretty prints an integral number with Base58 encoding
--   (utilizes `showIntAtBase` function from @Numeric@).
showIntegralInBase58 :: (Integral a, Show a) => a -> Either Text String
showIntegralInBase58 x
  -- {{{
  | x < 0     = Left "can't show a negative number in base58."
  | otherwise =
      Right $ showIntAtBase
        58
        (\ind -> chr $ fromIntegral (base58Chars !! ind))
        x
        ""
  -- }}}


-- | Converts an `Integer` to an array of bytes, all between @0x00@
--   and @0x39@ (@57@).
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


-- | Appends the first 4 bytes of the `hash256` of input `ByteString`
--   to the end before encoding it in Base58.
toBase58WithChecksum :: ByteString -> ByteString
toBase58WithChecksum bs =
  -- {{{
  let
    cs = LBS.take 4 $ hash256 bs
    tier1 = bs <> cs
  in
  encodeBase58 tier1
  -- }}}


-- | After decoding the input, checks the embedded checksum
--   and in case of success, returns the original `ByteString`.
decodeBase58WithChecksum :: ByteString -> Either Text ByteString
decodeBase58WithChecksum bs = do
  -- {{{
  decoded <- decodeBase58 bs
  let (mainBytes, fromHash) = LBS.splitAt (LBS.length decoded - 4) decoded
      hashOfMain = hash256 mainBytes
  if fromHash == LBS.take 4 hashOfMain then
    return mainBytes
  else
    Left "bad format."
  -- }}}


-- | Pretty prints a Base58 encode `ByteString`.
showBase58EncodedBS :: ByteString -> Either Text String
showBase58EncodedBS bs =
    LBS.unpack bs
  & traverse ((chr . fromIntegral <$>) . (base58Chars !?) . fromIntegral)
  & explainMaybe "given bytestring does not have a valid base58 encoding."


-- From the original "haskoin" project.
---------------------------------------
-- | Converts a lazy `ByteString` to integer. Assumes big-endianness.
bsToInteger :: ByteString -> Integer
bsToInteger =
  -- {{{
  bsToIntegerHelper True
  -- }}}


-- | Converts a lazy `ByteString` to integer. Assumes little-endianness.
bsToIntegerLE :: ByteString -> Integer
bsToIntegerLE =
  -- {{{
  bsToIntegerHelper False
  -- }}}


-- | Helper function. Return numbers are greater than or equal to @0@.
bsToIntegerHelper :: Bool -> ByteString -> Integer
bsToIntegerHelper be =
  -- {{{
  let
    f w n = toInteger w .|. shiftL n 8
  in
  LBS.foldr f 0 . (if be then LBS.invForBE else LBS.invForLE)
  -- }}}


-- | Helper function for converstion of `ByteString` values to signed
--   `Integral` values.
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


-- | Converts a lazy `ByteString` to an `Integral`. Assumes big-endianness.
bsToSignedIntegral :: Integral a => ByteString -> a
bsToSignedIntegral   = bsToSignedIntegralHelper True


-- | Converts a lazy `ByteString` to an `Integral`. Assumes little-endianness.
bsToSignedIntegralLE :: Integral a => ByteString -> a
bsToSignedIntegralLE = bsToSignedIntegralHelper False


-- | Converts an `Integer` to a `ByteString` (big-endian).
integerToBS :: Integer -> ByteString
integerToBS =
  -- {{{
  integerToBSHelper True
  -- }}}


-- | Converts an `Integer` to a `ByteString` (little-endian).
integerToBSLE :: Integer -> ByteString
integerToBSLE =
  -- {{{
  integerToBSHelper False
  -- }}}


-- | Helper function for conversion of an `Integer` values to a `ByteString`.
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
      LBS.singleton 0x00
      -- }}}
  -- }}}


-- | Helper function for conversion of signed `Integral` values
--   to `ByteString` values.
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


-- | Converts a signed `Integral` value to `ByteString` (big-endian).
signedIntegralToBS :: Integral a => a -> ByteString
signedIntegralToBS = signedIntegralToBSHelper True


-- | Converts a signed `Integral` value to `ByteString` (little-endian).
signedIntegralToBSLE :: Integral a => a -> ByteString
signedIntegralToBSLE = signedIntegralToBSHelper False


-- | Encodes a lazy `ByteString` in Base16.
encodeHex :: ByteString -> ByteString
encodeHex = B16.encodeBase16'


-- | Encodes a lazy `ByteString` in Base16 (little-endian).
--   So far this function has proven useless.
encodeHexLE :: ByteString -> ByteString
encodeHexLE = encodeHex . LBS.invForLE
---------------------------------------


-- | Decodes a lazy `ByteString` from Base16.
decodeHex :: ByteString -> Either Text ByteString
decodeHex = B16.decodeBase16


-- | Decodes a lazy `ByteString` from Base16. (little-endian).
--   Similar to `encodeHexLE`, this too has proven useless so far.
decodeHexLE :: ByteString -> Either Text ByteString
decodeHexLE = decodeHex . LBS.invForLE


-- | A more generalized version of `integerToBS`.
integralToBS :: Integral n => n -> ByteString
integralToBS = integerToBS . toInteger


-- | A more generalized version of `integerToBSLE`.
integralToBSLE :: Integral n => n -> ByteString
integralToBSLE = integerToBSLE . toInteger


-- | Helper function. Ignores the given byte count if it's smaller
--   than the minimum required number of bytes.
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


-- | Converts an `Integral` value to a big-endian `ByteString`
--   with specific number of bytes at minimum.
integralToNBytes :: Integral n => Word -> n -> ByteString
integralToNBytes =
  -- {{{
  integralToNBytesHelper True
  -- }}}


-- | Converts an `Integral` value to a little-endian `ByteString`
--   with specific number of bytes at minimum.
integralToNBytesLE :: Integral n => Word -> n -> ByteString
integralToNBytesLE =
  -- {{{
  integralToNBytesHelper False
  -- }}}


-- | Converts an `Integral` to a big-endian `ByteString` with 32 bytes.
--   Results in longer `ByteString` if the number needs more bytes.
integralTo32Bytes :: Integral n => n -> ByteString
integralTo32Bytes =
  -- {{{
  integralToNBytes 32
  -- }}}


-- | Converts an `Integral` to a little-endian `ByteString` with 32 bytes.
--   Results in longer `ByteString` if the number needs more bytes.
integralTo32BytesLE :: Integral n => n -> ByteString
integralTo32BytesLE =
  -- {{{
  integralToNBytesLE 32
  -- }}}


-- | Attempts to convert a string that consists of @[0..9]@, @[a..f]@,
--   or @[A..F]@ to a lazy `ByteString`.
base16StringToBS :: String -> Either Text ByteString
base16StringToBS b16 =
  -- {{{
  B16.decodeBase16 (fromString b16)
  -- }}}


-- | After converting the given `Integer` to a big-endian `ByteString`,
--   prepends it with an optional byte.
prependIntegerWithWord8 :: Maybe Word8 -> Integer -> ByteString
prependIntegerWithWord8 mW8 n =
  -- {{{
  maybe LBS.empty LBS.singleton mW8 <> integerToBS n
  -- }}}


-- | SHA1 hashing of a lazy `ByteString`.
sha1 :: ByteString -> ByteString
sha1 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA1 . LBS.toStrict
  -- }}}


-- | SHA256 hashing of a lazy `ByteString`.
sha256 :: ByteString -> ByteString
sha256 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA256 . LBS.toStrict
  -- }}}

-- | RIPEMD160 hashing of a lazy `ByteString`.
ripemd160 :: ByteString -> ByteString
ripemd160 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith RIPEMD160 . LBS.toStrict
  -- }}}


-- | HASH160 hashing of a lazy `ByteString` (@SHA256@ and then @RIPEMD160@).
hash160 :: ByteString -> ByteString
hash160 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith RIPEMD160 . hashWith SHA256 . LBS.toStrict
  -- }}}


-- | HASH256 hashing of a lazy `ByteString` (@SHA256@ twice).
hash256 :: ByteString -> ByteString
hash256 =
  -- {{{
  LBS.fromStrict . BA.convert . hashWith SHA256 . hashWith SHA256 . LBS.toStrict
  -- }}}


-- | Converts a `Word8` into a list of `Bool` values, each representing a bit.
--   Least significant bit will sit at list's head.
word8ToBools :: Word8 -> [Bool]
word8ToBools w8 = 
  -- {{{
  map (testBit w8) [0..7]
  -- }}}


-- | Converts a list of `Bool` values (representing bits) to a byte.
--   Lists with less elements than 8 are "0-padded" to right, while
--   only the first 8 elements of bigger lists are considered..
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 xs =
  -- {{{
  foldl setBit 0 (map snd $ filter fst $ zip xs [0..7])
  -- }}}


-- | Splits a list into chunks of `Int` length. Last chunk may be smaller.
splitIn :: Int -> [a] -> [[a]]
splitIn _ [] = []
splitIn c xs =
  -- {{{
  let
    (xs1, xs2) = splitAt c xs
  in
  xs1 : splitIn c xs2
  -- }}} 


-- | Similar to `map`, but the mapping function also requires
--   the index of the element which is getting applied to.
indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs =
  -- {{{
  let
    go _     soFar []       = soFar
    go index soFar (y : ys) = go (index + 1) (f index y : soFar) ys
  in
  reverse $ go 0 [] xs
  -- }}}


-- | Similar to `indexedMap`, but the mapping function results
--   in a monadic value.
indexedMapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
indexedMapM fn xs = zipWithM fn [0 .. length xs - 1] xs


-- | If the given `Either` value is a `Right`, `Just` is returned.
--   `Nothing` otherwise.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe _         = Nothing


-- | From @extra@ package. Safe version of the indexing
--   operator `(!!)`.
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


-- | This should've been imported from @Control.Monad.IO.Class@,
--   but there seems to be an issue. So I've redefined it for the
--   time being.
hoistMaybe :: Applicative m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure


-- | If both `Either` values are `Right`, a `Just` tuple is return.
--   `Nothing` otherwise.
fromTwoEitherValues :: Either p x -> Either q y -> Maybe (x, y)
fromTwoEitherValues (Right x) (Right y) = Just (x, y)
fromTwoEitherValues _         _         = Nothing


-- | Converts a `Maybe` value into `Either` to help clarify the
--   cause of failure.
explainMaybe :: Text -> Maybe a -> Either Text a
explainMaybe explanation Nothing = Left explanation
explainMaybe _          (Just x) = Right x


mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft _ (Right x)  = Right x
mapLeft f (Left err) = Left $ f err


-- | Helper function for nonce generation.
getNByteNonce :: Num a => Int -> IO a
getNByteNonce n =
  fromInteger . bsToInteger . LBS.fromStrict <$> getRandomBytes n


-- | A more generic version of @getPOSIXTime@.
getPOSIX :: Num a => IO a
getPOSIX = fromIntegral . round <$> getPOSIXTime


-- | From base-4.16.1:
-- > clamp (low, high) a = min high (max a low)
--
-- Function for ensursing the value @a@ is within the inclusive bounds given by
-- @low@ and @high@. If it is, @a@ is returned unchanged. The result
-- is otherwise @low@ if @a <= low@, or @high@ if @high <= a@.
clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)


-- | My own version of the `trace` function that shows the
--   returned value after the given lable.
myTrace lbl x = trace (lbl ++ show x) x


-- }}}





