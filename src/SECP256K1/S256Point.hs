{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}


module SECP256K1.S256Point where


import           Debug.Trace              (trace)
import           Utils
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import qualified Data.ByteString.Lazy     as LBS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.String              as String
import           SECP256K1.Constants
import           SECP256K1.S256Field


type S256Point =
  FFEC.Point 115792089237316195423570985008687907853269984665640564039457584007908834671663
             0
             7


gx :: S256Field
gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
gy :: S256Field
gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
generator :: S256Point
generator = FFEC.unsafeFromCoords gx gy


toSEC :: Bool -> S256Point -> ByteString
toSEC compressed point =
  -- {{{
  case (FFEC.getX point, FFEC.getY point) of
    (Just x_, Just y_) ->
      let
        x = toInteger x_
        y = toInteger y_
        tier1 =
          if compressed then
            if even y then
              prependIntegerWithWord8 (Just 2) x
            else
              prependIntegerWithWord8 (Just 3) x
          else
            prependIntegerWithWord8 (Just 4) x <> integerToBS y
      in
      tier1
    _ ->
      LBS.empty
  -- }}}


address :: Bool -> Bool -> S256Point -> ByteString
address compressed testnet point =
  -- {{{
  let
    initBytes = if testnet then LBS.pack [0x6f] else LBS.pack [0x00]
    sec       = toSEC compressed point
    hashTier1 = initBytes <> hash160 sec
  in
  toBase58WithChecksum hashTier1
  -- }}}

