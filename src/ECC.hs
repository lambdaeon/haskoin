{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}


module ECC
  ( toSEC
  , secParser
  , address
  , addressToHash160OfSEC
  , pubKeyOf
  , wifOf
  , verify
  , signWith
  , PubKey
  , SecKey
  , SigHash
  , Signature (..)
  , Nonce
  , testnetPublicKey
  , testnetPrivateKey
  , testnetWallet
  , testnetChangePublicKey
  , testnetChangePrivateKey
  , testnetChangeWallet
  ) where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Char                   (ord)
import           Data.Serializable
import qualified Extension.ByteString.Lazy   as LBS
import           Extension.ByteString.Parser
import qualified FieldElement                as FE
import qualified FiniteFieldEllipticCurve    as FFEC
import           SECP256K1
import           TestnetWalletPassPhrase     (sourceForSecretKey, sourceForSecretKeyOfChangeAddress)
import qualified Text.Megaparsec             as P
import           Utils


-- DATATYPES
-- {{{
-- | Type alias for the publick key (a point on the
--   @SECP256K1@ curve).
type PubKey    = S256Point


-- | Type alias for the secret key (a scalar on the
--   finite field defined by the set from @SECP256K1@'s
--   order).
type SecKey    = S256Order


-- | Type alias for the signature hash. Signature hash is
--   the "fingerprint" of a message, which is basically
--   the HASH256 of it.
type SigHash   = S256Order


-- | A random unique value for making each `Signature`.
type Nonce     = S256Order


-- | Record type to represent a signature.
data Signature = Signature
  { r :: S256Field
  , s :: S256Order
  } deriving (Eq, Show)

instance Serializable Signature where
  -- | Serialization to DER format.
  serialize Signature {..} =
    -- {{{
    let
      prependLength bs =
        -- {{{
        let
          lenBS = LBS.singleton $ fromIntegral $ LBS.length bs
        in
        lenBS <> bs
        -- }}}
      fromInitBS bs =
        -- {{{
        let
          tier1 =
            if bsToInteger (LBS.take 1 bs) >= 0x80 then
              0x00 `LBS.cons'` bs
            else
              bs
        in
        0x02 `LBS.cons'` prependLength tier1
        -- }}}
      rBS = fromInitBS $ integralToBS r
      sBS = fromInitBS $ integralToBS s
    in
    0x30 `LBS.cons'` prependLength (rBS <> sBS)
    -- }}}
  -- | Parses a DER format serialization.
  parser =
    -- {{{
    let
      innerParser :: Num a => Parser a
      innerParser = do
        -- {{{
        void $ P.single 0x02
        len <- fromIntegral <$> P.anySingle
        P.lookAhead $ P.takeP (Just "checking for the correct byte count") len
        fstByte <- P.anySingle
        rest    <- P.takeP Nothing (len - 1)
        if fstByte == 0x00 && len >= 2 then do
          return $ fromInteger $ bsToInteger rest
        else
          return $ fromInteger $ bsToInteger $ fstByte `LBS.cons'` rest
        -- }}}
    in do
    void $ P.single 0x30
    len0 <- fromIntegral <$> P.anySingle
    P.lookAhead $ P.takeP (Just "checking for the correct byte count") len0
    r <- innerParser
    s <- innerParser
    return $ Signature {..}
    -- }}}
-- }}}


-- | Serializes a `PubKey` according to the SEC standard.
toSEC :: Bool -> PubKey -> ByteString
toSEC compressed point =
  -- {{{
  case (FFEC.getX point, FFEC.getY point) of
    (Right x_, Right y_) ->
      let
        x = toInteger x_
        y = toInteger y_
      in
      if compressed then
        if even y then
          prependIntegerWithWord8 (Just 2) x
        else
          prependIntegerWithWord8 (Just 3) x
      else
        prependIntegerWithWord8 (Just 4) x <> integerToBS y
    _ ->
      LBS.empty
  -- }}}


-- | Parses a `PubKey` serialized with SEC standard.
secParser :: Parser PubKey
secParser = do
  -- {{{
  fstByte <- P.anySingle
  x       <- fromInteger . bsToInteger <$> P.takeP (Just "x bytes") 32
  let yFromX x =
        -- {{{
        s256FieldSqrt $ x ^ 3 + fromInteger a * x + fromInteger b
        -- }}}
      fromXY x' y' =
        -- {{{
        case FFEC.fromCoords x' y' of
          Right point ->
            return point
          Left _ ->
            fail "invalid point"
        -- }}}
  if fstByte == 0x02 then do
    -- {{{
    let initY = yFromX x
        y = if even initY then (-initY) else initY
    fromXY x y
    -- }}}
  else if fstByte == 0x03 then do
    -- {{{
    let initY = yFromX x
        y = if even initY then initY else (-initY)
    fromXY x y
    -- }}}
  else if fstByte == 0x04 then do
    -- {{{
    y <- fromInteger . bsToInteger <$> P.takeP (Just "y bytes") 32
    fromXY x y
    -- }}}
  else
    -- {{{
    fail "bad format: SEC"
    -- }}}
  -- }}}


-- | Converts the public key point to its "human readable"
--   Bitcoin address (encoded in Base58).
address :: Bool -> Bool -> PubKey -> ByteString
address compressed testnet point =
  -- {{{
  let
    initBytes = if testnet then LBS.singleton 0x6f else LBS.singleton 0x00
    sec       = toSEC compressed point
    hashTier1 = initBytes <> hash160 sec
  in
  toBase58WithChecksum hashTier1
  -- }}}


-- | Decodes a Bitcoin address to the HASH160 of its
--   SEC formatted public key point.
addressToHash160OfSEC :: ByteString -> Either Text ByteString
addressToHash160OfSEC addr58 = do
  -- {{{
  decoded <- decodeBase58WithChecksum addr58
  return $ LBS.drop 1 decoded
  -- }}}


-- | Returns the public key point corresponding to a secret key.
pubKeyOf :: SecKey -> PubKey
pubKeyOf e = FFEC.scaleBy (toInteger e) generator


-- | Encodes a secret key to a Wallet Import Format (WIF).
wifOf :: Bool -> Bool -> SecKey -> ByteString
wifOf compressed testnet e =
  -- {{{
  let
    eBS = integralTo32Bytes e
    pre = LBS.singleton $ if testnet then 0xef else 0x80
    suf = LBS.pack [0x01 | compressed]
  in
  toBase58WithChecksum $ pre <> eBS <> suf
  -- }}}


-- | `Signature` verification from `PubKey` and `SigHash`.
verify :: PubKey -> SigHash -> Signature -> Bool
verify pubPoint z_ Signature {r = r_, s = s} =
  -- {{{
  let
    z    = toInteger z_
    r    = toInteger r_
    sInv = toInteger (1 `div` s)
    u    = mod (z * sInv) n
    v    = mod (r * sInv) n
    uG   = FFEC.scaleBy u generator
    vP   = FFEC.scaleBy v pubPoint 
    uGvP = uG <> vP
  in
  case FFEC.getX uGvP of
    Right r' ->
      r == toInteger r'
    Left _ ->
      False
  -- }}}


-- | Siging a `SigHash` with a `SecKey` and a `Nonce`.
signWith :: SecKey -> Nonce -> SigHash -> Either Text Signature
signWith e_ k_ z_ = do
  -- {{{
  let e = toInteger e_
      k = toInteger k_
      z = toInteger z_
      kInv = toInteger $ 1 `div` k_
  r_ <- FFEC.getX $ FFEC.scaleBy k generator
  let r = toInteger r_
      s = fromInteger $ (z + r * e) * kInv
  return $ Signature
    { r = r_
    , s = s
    }
  -- }}}


-- ** Testnet Wallet

makePrivateKeyFromSecretBS :: ByteString -> SecKey
makePrivateKeyFromSecretBS = fromInteger . bsToIntegerLE . hash256


testnetPublicKey :: PubKey
testnetPublicKey = pubKeyOf testnetPrivateKey


testnetPrivateKey :: SecKey
testnetPrivateKey =
  -- {{{
  makePrivateKeyFromSecretBS sourceForSecretKey
  -- }}}


testnetChangePrivateKey :: SecKey
testnetChangePrivateKey =
  -- {{{
  makePrivateKeyFromSecretBS sourceForSecretKeyOfChangeAddress
  -- }}}


testnetWallet :: ByteString
testnetWallet =
  -- {{{
  address True True testnetPublicKey
  -- }}}


testnetChangePublicKey :: PubKey
testnetChangePublicKey = pubKeyOf testnetChangePrivateKey

testnetChangeWallet :: ByteString
testnetChangeWallet =
  -- {{{
  address True True testnetChangePublicKey
  -- }}}



