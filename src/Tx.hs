-- MODULE
-- {{{
module Tx
  ( Tx (..)
  , getId
  , fetch
  , fee
  , getTxInsTxOut
  , getTxInAmount
  , sigHashForTxIn
  , verifyFor
  , verifyTxInFor
  , getCoinbaseTxIn
  , coinbaseHeight
  , sampleTxBS
  , sampleP2SH
  ) where
-- }}}


-- IMPORTS
-- {{{
import           Debug.Trace                 (trace)
import           Control.Monad               (replicateM, forM, when)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString             as BS
import           Data.Char                   (chr)
import           Data.Function               ((&))
import           Data.Functor                (void)
import           Data.Serializable
import           Data.String                 (fromString)
import           Data.Varint                 (Varint (..))
import qualified Data.Varint                 as Varint
import           Data.Void
import           Data.Word                   (Word32)
import           Extension.ByteString.Parser  
import qualified Extension.ByteString.Lazy   as LBS
import           Locktime                    (Locktime)
import qualified Locktime
import           Network.HTTP.Simple         (httpLbs, getResponseBody, Request)
import           Script                      (RedeemScript)
import qualified Script
import           ECC                         (SigHash)
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Debug       as P
import           TxIn                        (TxIn (..))
import qualified TxIn
import           TxOut                       (TxOut (..))
import qualified TxOut
import           Utils
-- }}}



-- | Record type to represent a transaction.
data Tx = Tx
  { txVersion  :: Word32
  , txTxIns    :: [TxIn]
  , txTxOuts   :: [TxOut]
  , txLocktime :: Locktime
  , txTestnet  :: Bool -- ^ Whether the transaction is meant for the testnet.
  } deriving (Eq, Show)


-- | HASH256 of a serialized transaction.
getId :: Tx -> ByteString
getId = hash256 . serialize


instance Serializable Tx where
  serialize tx =
    -- {{{
    serializeWithCustomTxIns (Varint.serializeList $ txTxIns tx) tx
    -- }}}
  parser = do
    -- {{{
    txVersion  <- word32ParserLE "version"
    when (txVersion /= 1)
      $ void
      $ P.takeP
          (Just "found the need for this discard of 2 bytes by trial and error")
          2
    numTxIns   <- Varint.countParser
    txTxIns    <- replicateM numTxIns  parser
    numTxOuts  <- Varint.countParser
    txTxOuts   <- replicateM numTxOuts parser
    txLocktime <- parser
    let txTestnet = False
    return $ Tx {..}
    -- }}}


-- | Helper function to allow the creation of "intermediate" transactions
--   used for generating `SigHash` values, or verifying scripts of `TxIn`
--   values..
serializeWithCustomTxIns :: ByteString -> Tx -> ByteString
serializeWithCustomTxIns bs Tx {..} =
  -- {{{
     integralToNBytesLE 4 txVersion
  <> bs
  <> Varint.serializeList txTxOuts
  <> serialize            txLocktime
  -- }}}


-- | Fee computation of a transaction (total in, minus total out).
fee :: Tx -> ExceptT Text IO Word
fee Tx {..} = do
  -- {{{
  let totOut = foldr (\txOut acc -> txOutAmount txOut + acc) 0 txTxOuts
  words <- forM txTxIns (getTxInAmount txTestnet)
  return $ sum words - totOut
  -- }}}


-- | Verification of a transaction with respect to the given script scheme
--   by making sure the fee is greater than or equal to 0, and that all
--   `TxIn` values are also valid.
verifyFor :: Script.Scheme -> Tx -> ExceptT Text IO ()
verifyFor scheme tx@Tx {..} = do
  -- {{{
  fee' <- fee tx
  if fee' >= 0 then
    mapM_ (verifyTxInFor scheme tx) txTxIns
  else
    fail "negative fee."
  -- }}}


-- | Generates the signature hash ("fingerprint" of the signed message)
--   of the transaction for a specific `TxIn` of it.
--
--     (1) Goes through all the `TxIn` values and serializes them with
--         @0x00@ instead of their own `ScriptSig`.
--
--     2.  When it reaches the `TxIn` in question:
--
--           * For p2pkh, looks up the block explorer (if no `TxOut` is
--             provided), takes the corresponding `TxOut`'s `ScriptPubKey`
--             and used its serialization to serialize the `TxIn`.
--
--           * For p2sh, attempts to find the RedeemScript from its own
--             `ScriptSig`, and put its serialization in place of the
--             empty script.
--
--     3.  Serializes the `Tx` with this customized serialization of the `TxIn`
--         values.
--
--     4.  Appends @SIGHASH_ALL@ with @0x01@ in 4 little-endian bytes to the
--         custom serialization of the `Tx`.
--
--     5.  Performs a `hash256` on the result.
--
--     6.  Converts this hashed bytestring to the `SigHash` value (which
--         is a `S256Order` value, which in turn is a `FieldElement` with
--         the prime of `n` from @SECP256K1@). This `Num` is interpreted
--         from the `ByteString` as big-endian.
sigHashForTxIn :: Tx
               -> TxIn
               -> Either RedeemScript (Maybe TxOut) -- ^ Option to choose between p2sh and p2pkh. `TxOut` may be provided as a (possibly temporary) caching mechanism.
               -> ExceptT Text IO SigHash
sigHashForTxIn tx@Tx {..} txIn p2shOrP2PKH = do
  -- {{{
  txInsBSs <- mapM
                ( \txIn' ->
                    if txIn == txIn' then
                      case p2shOrP2PKH of
                        Left redeemScript ->
                          -- {{{
                          return $
                            TxIn.serializeWithCustomScriptSig
                              (serialize redeemScript)
                              txIn
                          -- }}}
                        Right mTxOutCache -> do
                          -- {{{
                          txOut <- case mTxOutCache of
                                      Just txOutCache  ->
                                        return txOutCache
                                      Nothing ->
                                        getTxInsTxOut txTestnet txIn
                          return $
                            TxIn.serializeWithCustomScriptSig
                              (serialize $ txOutScriptPubKey txOut)
                              txIn
                          -- }}}
                    else
                      return $ TxIn.serializeWithoutScriptSig txIn
                )
                txTxIns
  let txInCount  :: Varint
      txInCount  = Varint $ fromIntegral $ length txTxIns
      txInsBS    = serialize txInCount <> LBS.concat txInsBSs
      beforeHash =
           serializeWithCustomTxIns txInsBS tx
        <> integralToNBytesLE 4 1 -- SIGHASH_ALL at the end.
  return $ fromInteger $ bsToInteger $ hash256 beforeHash
  -- }}}


-- | Verifies a specific `TxIn` of a transaction.
--
--     (1) Finds the `SigHash` of the `TxIn` in question based on the scheme.
--
--     2.  Uses this `SigHash` to verify the stack of its
--         `ScriptSig` atop its corresponding `ScriptPubKey`.
verifyTxInFor :: Script.Scheme -> Tx -> TxIn -> ExceptT Text IO ()
verifyTxInFor scheme tx@Tx {..} txIn = do
  -- {{{
  txOut   <- getTxInsTxOut txTestnet txIn
  sigHash <- case scheme of
               Script.P2PKH ->
                 sigHashForTxIn tx txIn (Right $ Just txOut)
               Script.P2SH -> do
                 redeemScript <- Script.getRedeemScript
                                   (txInScriptSig     txIn )
                                   (txOutScriptPubKey txOut)
                                 & except
                 sigHashForTxIn tx txIn (Left redeemScript)
  except $ Script.validate
    (txInScriptSig     txIn )
    (txOutScriptPubKey txOut)
    sigHash
  -- }}}


-- | Fetches a transaction from its ID from a block explorer
--   (<https://blockstream.info>). Reverses the given ID for query.
fetch :: Bool -> ByteString -> ExceptT Text IO Tx
fetch testnet txId =
  -- {{{
  let
    baseEndPoint
      | testnet   = "https://blockstream.info/testnet/api/"
      | otherwise = "https://blockstream.info/api/"
    url :: Request
    url =   baseEndPoint <> "tx/" <> encodeHex txId <> "/hex"
          & LBS.unpack
          & map (chr . fromIntegral)
          & fromString
    refineTx tx = tx
      { txTestnet = testnet
      , txTxIns   = map TxIn.reversePrevId (txTxIns tx)
      }
  in do
  response <- liftIO $ httpLbs url
  resBS    <-   getResponseBody response
              & decodeHex
              & except
  parse resBS
    & fmap refineTx
    & except
    & withExceptT (const "failed to parse fetched tx.")
  -- }}}


-- | Fetches the `TxOut` which a `TxIn` points to.
getTxInsTxOut :: Bool -> TxIn -> ExceptT Text IO TxOut
getTxInsTxOut testnet TxIn {..} = do
  -- {{{
  tx <- fetch testnet txInPrevTx
  let txOuts    = txTxOuts tx
      outsCount = fromIntegral $ length txOuts
      prevIndex = fromIntegral txInPrevIndex
  if txInPrevIndex < outsCount then do
    return $ txOuts !! prevIndex
  else do
    fail "invalid txout index."
  -- }}}


-- | Fetches the amount of satoshis available in a specific `TxIn`.
getTxInAmount :: Bool -> TxIn -> ExceptT Text IO Word
getTxInAmount testnet txIn = do
  -- {{{
  txOut <- getTxInsTxOut testnet txIn
  return $ txOutAmount txOut
  -- }}}



-- ** Coinbase

-- | Determines whether a `Tx` is a coinbase transaction. Returns the
--   possible lone `TxIn` for convenience. There are 3 predicates:
--
--     (1) The transaction must have only one input.
--
--     2.  The previous transaction ID this `TxIn` points to
--         must be 32 bytes of @0x00@.
--
--     3.  Its output index must be @0xffffffff@ (max bound of `Word32`).
getCoinbaseTxIn :: Tx -> Maybe TxIn
getCoinbaseTxIn Tx {..} =
  -- {{{
  case txTxIns of
    [txIn@TxIn {..}]
      |    txInPrevTx    == LBS.replicate 32 0x00
        && txInPrevIndex == maxBound ->
      Just txIn
    _ ->
      Nothing
  -- }}}


-- | Get the block number of a coinbase transaction.
coinbaseHeight :: Tx -> Either Text Integer
coinbaseHeight tx@Tx {..} =
  -- {{{
  case getCoinbaseTxIn tx of
    Just txIn@TxIn {..} ->
      -- {{{
      case txInScriptSig of
        Script.Script (Script.Element heightBS : _) ->
          -- {{{
          Right $ bsToIntegerLE heightBS
          -- }}}
        _                                           ->
          -- {{{
          Left "invalid coinbase scriptsig."
          -- }}}
      -- }}}
    Nothing             ->
      -- {{{
      Left "not a coinbase transaction."
      -- }}}
  -- }}}



-- SAMPLE VALUE
-- {{{

-- === Tx =====
-- version:      0x01000000
--
-- txin count:   0x01
-- === TxIn ===
-- prev tx id:   0x813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1
-- prev index:   0x00000000
-- script sig:   0x6b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf2132060277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a
-- sequence:     0xfeffffff
--
-- txout count:  0x02
-- === TxOut ==
-- amount:       0xa135ef0100000000
-- scriptpubkey: 0x1976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac
-- txout1        0x99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac
--
-- locktime:     0x19430600


sampleTxBS :: ByteString
sampleTxBS =
     integralToNBytes 4 0x01000000
  <> serialize (Varint 1)
  <> TxIn.sampleTxIn
  <> serialize (Varint 2)
  <> TxOut.sampleTxOut1
  <> TxOut.sampleTxOut2
  <> integralToNBytes 4 0x19430600


sampleP2SH :: ByteString
sampleP2SH = integerToBS 0x0100000001868278ed6ddfb6c1ed3ad5f8181eb0c7a385aa0836f01d5e4789e6bd304d87221a000000475221022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb702103b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb7152aeffffffff04d3b11400000000001976a914904a49878c0adfc3aa05de7afad2cc15f483a56a88ac7f400900000000001976a914418327e3f3dda4cf5b9089325a4b95abdfa0334088ac722c0c00000000001976a914ba35042cfe9fc66fd35ac2224eebdafd1028ad2788acdc4ace020000000017a91474d691da1574e6b3c192ecfb52cc8984ee7b6c56870000000001000000
-- }}}
