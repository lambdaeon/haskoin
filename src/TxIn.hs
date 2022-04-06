module TxIn
  ( TxIn (..)
  , serializeWithCustomScriptSig
  , serializeWithoutScriptSig
  , reversePrevId
  , initWithEmptyScriptSig
  , defaultSequence
  , sampleTxIn
  ) where


import qualified Data.ByteString.Lazy        as LBS
import           Data.Serializable
import           Extension.ByteString.Parser 
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           Script                      (ScriptSig)
import qualified Script   
import           Utils



-- | Record type representing an input to a transaction.
data TxIn = TxIn
  { txInPrevTx    :: ByteString
  , txInPrevIndex :: Word32
  , txInScriptSig :: ScriptSig
  , txInSequence  :: Word32
  }

instance Eq TxIn where
  -- {{{
  -- | Equality check is done only on the previous transaction's
  --   ID, and the transaction output this input points to.
  (TxIn prevTx0 prevIndex0 _ _) == (TxIn prevTx1 prevIndex1 _ _) =
    prevTx0 == prevTx1 && prevIndex0 == prevIndex1
  -- }}}

instance Show TxIn where
  -- {{{
  show TxIn{..} =
       "TxIn {"
    ++ map
         (chr . fromIntegral)
         ( LBS.unpack $
                encodeHex txInPrevTx
             <> ":"
             <> encodeHex (integralToBSLE txInPrevIndex)
             <> ", "
         )
    ++ show txInScriptSig
    ++ "}"
  -- }}}

instance Serializable TxIn where
  serialize txIn =
    -- {{{
    serializeWithCustomScriptSig (serialize $ txInScriptSig txIn) txIn
    -- }}}
  parser = do
    -- {{{
    txInPrevTx     <- P.takeP (Just "prev tx") 32
    txInPrevIndex  <- word32ParserLE "prev index"
    txInScriptSig  <- parser
    txInSequence   <- word32ParserLE "sequence"  
    return $ TxIn {..}
    -- }}}


-- | Helper serialization function to allow different serialization
--   scheme to accomodate `SigHash` generation and script verification.
serializeWithCustomScriptSig :: ByteString -> TxIn -> ByteString
serializeWithCustomScriptSig customSS TxIn {..} =
  -- {{{
     LBS.reverse txInPrevTx
  <> integralToNBytesLE 4 txInPrevIndex
  <> customSS
  <> integralToNBytesLE 4 txInSequence
  -- }}}


-- | Special serialization scheme needed and an intermediate step of
--   `SigHash` generation.
serializeWithoutScriptSig :: TxIn -> ByteString
serializeWithoutScriptSig =
  -- {{{
  serializeWithCustomScriptSig $ LBS.singleton 0
  -- }}}


-- | Reverses the previous transaction's ID which the given `TxIn` value
--   points to.
reversePrevId :: TxIn -> TxIn
reversePrevId txIn@TxIn {..} =
  -- {{{
  txIn {txInPrevTx = LBS.reverse txInPrevTx}
  -- }}}



-- | A transaction input from a previous transaction's ID, along with
--   an output index, with an empty script and `defaultSequence`.
initWithEmptyScriptSig :: ByteString -> Word32 -> TxIn
initWithEmptyScriptSig prevTx prevIndex =
  -- {{{
  TxIn
    { txInPrevTx    = prevTx
    , txInPrevIndex = prevIndex
    , txInScriptSig = mempty
    , txInSequence  = defaultSequence
    }
  -- }}}


-- | A default value for the sequence field of a `TxIn`. Probably temporary.
defaultSequence :: Word32
defaultSequence = 0xffffffff



sampleTxIn :: ByteString
sampleTxIn =
  -- {{{
  let
    prevTx    = integerToBS 0x813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1
    prevIndex = integralToNBytes 4 0x00000000
    scriptSig = Script.sampleScript0BS
    sequence  = integralToNBytes 4 0xfeffffff
  in
  prevTx <> prevIndex <> scriptSig <> sequence
  -- }}}
