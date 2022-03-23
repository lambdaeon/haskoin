{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module TxIn
  ( TxIn (..)
  , sampleTxIn
  ) where


import           Debug.Trace
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Serializable
import           Data.Word                   (Word32)
import           Extension.ByteString.Parser 
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           Script                      (ScriptSig)
import qualified Script   
import           Utils



sampleTxIn :: ByteString
sampleTxIn =
  -- {{{
  let
    prevTx    = integerToBS 0x813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1
    prevIndex = integralToNBytes 4 0x00000000
    scriptSig = Script.sampleStack0BS
    sequence  = integralToNBytes 4 0xfeffffff
  in
  prevTx <> prevIndex <> scriptSig <> sequence
  -- }}}



data TxIn = TxIn
  { txInPrevTx    :: ByteString
  , txInPrevIndex :: Word32
  , txInScriptSig :: ScriptSig
  , txInSequence  :: Word32
  } deriving (Eq)

instance Show TxIn where
  -- {{{
  show TxIn{..} = show $
       encodeHex txInPrevTx
    <> ":"
    <> encodeHex (integralToBSLE txInPrevIndex)
  -- }}}

instance Serializable TxIn where
  serialize TxIn {..} =
    -- {{{
       txInPrevTx
    <> integralToNBytesLE 4 txInPrevIndex
    <> serialize txInScriptSig
    <> integralToNBytesLE 4 txInSequence
    -- }}}
  parser = do
    -- {{{
    txInPrevTx     <- P.takeP (Just "prev tx") 32
    txInPrevIndex  <- word32ParserLE "prev index"
    txInScriptSig  <- parser
    txInSequence   <- word32ParserLE "sequence"  
    return $ TxIn {..}
  -- }}}
