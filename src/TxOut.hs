{-# LANGUAGE RecordWildCards   #-}


module TxOut
  ( TxOut (..)
  , sampleTxOut1
  , sampleTxOut2
  ) where


import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Serializable
import           Data.Word                   (Word)
import           Extension.ByteString.Parser 
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           Script                      (ScriptPubKey)
import qualified Script      
import           Utils


data TxOut = TxOut
  { txOutAmount       :: Word
  , txOutScriptPubKey :: ScriptPubKey
  } deriving (Eq, Show)


instance Serializable TxOut where
  serialize TxOut {..} =
    -- {{{
       integralToNBytesLE 8 txOutAmount
    <> serialize txOutScriptPubKey
    -- }}}
  parser = do
    -- {{{
    txOutAmount       <- P.dbg "TXOUT AMOUNT" $ fromInteger . bsToIntegerLE <$> P.takeP (Just "amount") 8
    txOutScriptPubKey <- P.dbg "TXOUT SCRIPTPUBKEY" parser
    return $ TxOut {..}
    -- }}}


-- SAMPLE VALUES
-- {{{
sampleTxOut1, sampleTxOut2 :: ByteString
sampleTxOut1 =
  -- {{{
  let
    amount   = integralToNBytes 8 0xa135ef0100000000
    scriptPK = Script.sampleStack1BS
  in
  amount <> scriptPK
  -- }}}
sampleTxOut2 =
  -- {{{
  let
    amount   = integralToNBytes 8 0x99c3980000000000
    scriptPK = Script.sampleStack2BS
  in
  amount <> scriptPK
  -- }}}
-- }}}


