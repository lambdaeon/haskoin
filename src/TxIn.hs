{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module TxIn
  ( TxIn (..)
  , serializeWithCustomScriptSig
  , serializeWithoutScriptSig
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




data TxIn = TxIn
  { txInPrevTx    :: ByteString
  , txInPrevIndex :: Word32
  , txInScriptSig :: ScriptSig
  , txInSequence  :: Word32
  }

instance Eq TxIn where
  -- {{{
  (TxIn prevTx0 prevIndex0 _ _) == (TxIn prevTx1 prevIndex1 _ _) =
    prevTx0 == prevTx1 && prevIndex0 == prevIndex1
  -- }}}

instance Show TxIn where
  -- {{{
  show TxIn{..} = show $
       encodeHex txInPrevTx
    <> ":"
    <> encodeHex (integralToBSLE txInPrevIndex)
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


serializeWithCustomScriptSig :: ByteString -> TxIn -> ByteString
serializeWithCustomScriptSig customSS TxIn {..} =
  -- {{{
     txInPrevTx
  <> integralToNBytesLE 4 txInPrevIndex
  <> customSS
  <> integralToNBytesLE 4 txInSequence
  -- }}}


serializeWithoutScriptSig :: TxIn -> ByteString
serializeWithoutScriptSig =
  -- {{{
  serializeWithCustomScriptSig $ LBS.singleton 0
  -- }}}


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


-- probably temporary
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
