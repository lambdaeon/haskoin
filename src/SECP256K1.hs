{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module SECP256K1
  ( n
  , p
  , generator
  , verify
  , signWith
  ) where


import Debug.Trace (trace)
import Data.Maybe (fromJust)
import qualified FieldElement as FE
import qualified FiniteFieldEllipticCurve as FFEC
import FiniteCyclicGroup


-- UTILS
-- {{{
gx :: S256Field
gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
gy :: S256Field
gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
generator :: S256Point
generator = FFEC.unsafeFromCoords gx gy
n  :: Integer
n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
p  :: Integer
p  = 2 ^ 256 - 2 ^ 32 - 977
--                                                        p
--                               v----------------------------------------------------------------------------v
type S256Field = FE.FieldElement 115792089237316195423570985008687907853269984665640564039457584007908834671663
type S256Point = FFEC.Point      115792089237316195423570985008687907853269984665640564039457584007908834671663 0 7
type S256Order = FE.FieldElement 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
type PubKey    = S256Point
type SecKey    = S256Order
type Message   = S256Order
type Nonce     = S256Order
type Signature = (S256Field, S256Order)
-- }}}


verify :: PubKey -> Message -> Signature -> Bool
verify pubPoint z_ (r_, s) =
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
  return (r_, s)
  -- }}}



