{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}


module SECP256K1.Constants where


import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC


type S256Order = FE.FieldElement 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141


n  :: Integer
n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
p  :: Integer
p  = 2 ^ 256 - 2 ^ 32 - 977
