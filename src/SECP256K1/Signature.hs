{-# LANGUAGE RecordWildCards #-}


module SECP256K1.Signature where


import           Data.Function            ((&))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified FieldElement             as FE
import qualified FiniteFieldEllipticCurve as FFEC
import           Utils
import           SECP256K1.Constants
import           SECP256K1.S256Field
import           SECP256K1.S256Point


data Signature = Signature
  { r :: S256Field
  , s :: S256Order
  } deriving (Eq, Show)


toDER :: Signature -> ByteString
toDER Signature {..} =
  -- {{{
  let
    prependLength bs =
      -- {{{
      let
        lenBS = BS.pack [fromIntegral $ BS.length bs]
      in
      lenBS <> bs
      -- }}}
    fromInitBS bs =
      -- {{{
      let
        tier1 =
          if bsToInteger (BS.take 1 bs) >= 0x80 then
            BS.pack [0x00] <> bs
          else
            bs
      in
      BS.pack [0x02] <> prependLength tier1
      -- }}}
    rBS = fromInitBS $ integralToBS r
    sBS = fromInitBS $ integralToBS s
    finalBS = BS.pack [0x30] <> prependLength (rBS <> sBS)
  in
  encodeHex finalBS
  -- }}}









