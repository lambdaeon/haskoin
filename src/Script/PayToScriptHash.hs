module Script.PayToScriptHash where


import           Crypto.Random           (getRandomBytes)
import qualified Data.ByteString.Lazy as LBS
import           Data.Serializable
import qualified Locktime
import           Script
import           ECC                     (signWith, addressToHash160OfSEC)
import qualified ECC
import           TxIn                    (TxIn (..))
import qualified TxIn
import           TxOut                   (TxOut (..))
import qualified TxOut
import           Tx                      (Tx (..))
import qualified Tx
import           Utils


-- | From a public key point, to a Base58 encoded (with checksum)
--   p2sh address. The @Bool@ indicates whether the address is
--   meant for testnet or not.
hash160ToAddress :: Bool -> ByteString -> ByteString
hash160ToAddress testnet h160 =
  -- {{{
  let
    initBytes =
      if testnet then
        LBS.singleton 0xc4
      else
        LBS.singleton 0x05
  in
  toBase58WithChecksum $ initBytes <> h160
  -- }}}



