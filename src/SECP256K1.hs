{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}


module SECP256K1
  ( n
  , p
  , generator
  , pubKeyOf
  , wifOf
  , verify
  , signWith
  , testnetWallet
  ) where


import           Debug.Trace              (trace)
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
import           FiniteCyclicGroup
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import           Utils
import           SECP256K1.Constants
import           SECP256K1.Signature
import           SECP256K1.S256Field
import           SECP256K1.S256Point
import           TestnetWalletPassPhrase (sourceForSecretKey)


-- UTILS
-- {{{
type PubKey    = S256Point
type SecKey    = S256Order
type Message   = S256Order
type Nonce     = S256Order
-- }}}


pubKeyOf :: SecKey -> PubKey
pubKeyOf e = FFEC.scaleBy (toInteger e) generator

wifOf :: Bool -> Bool -> SecKey -> ByteString
wifOf compressed testnet e =
  -- {{{
  let
    eBS = integralTo32Bytes e
    pre = LBS.pack $ if testnet then [0xef] else [0x80]
    suf = LBS.pack [0x01 | compressed]
  in
  toBase58WithChecksum $ pre <> eBS <> suf
  -- }}}

verify :: PubKey -> Message -> Signature -> Bool
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
    Just r' ->
      r == toInteger r'
    Nothing ->
      False
  -- }}}


signWith :: SecKey -> Nonce -> Message -> Maybe Signature
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


testnetWallet :: ByteString
testnetWallet =
  -- {{{
  let
    sec = fromInteger $ bsToIntegerLE $ hash256 sourceForSecretKey
    pub = pubKeyOf sec
  in
  address True True pub
  -- }}}




