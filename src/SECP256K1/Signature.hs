{-# LANGUAGE RecordWildCards #-}


module SECP256K1.Signature where


import           Data.Function               ((&))
import           Data.Functor                (void)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import           Extension.ByteString.Parser  
import qualified FieldElement                as FE
import qualified FiniteFieldEllipticCurve    as FFEC
import           Utils
import           SECP256K1.Constants
import           SECP256K1.S256Field
import           SECP256K1.S256Point
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P


data Signature = Signature
  { r :: S256Field
  , s :: S256Order
  } deriving (Eq, Show)

instance Serializable Signature where
  serialize Signature {..} = -- Serialize to DER:
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
  parser = -- Parse from DER format:
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






