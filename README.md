# Haskoin: Bitcoin in Haskell
Bitcoin implementation in Haskell based on the book [Programming Bitcoin: Learn How to Program Bitcoin from Scratch](https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/) by [Jimmy Song](https://programmingbitcoin.com/).

## Running the Tests
The easiest way is to use `nix`.
  1. Run `nix-shell` (it'll use `shell.nix`),
  2. Use the accompanied shell script (`start.sh`) to run tests locally:
       - Run `./start.sh localTest all` to have all the tests executed,
       - or run `./start.sh localTest 10 2` for example, to test the
         solution for chapter 10's second exercise.

## Library Directory Structure

```
src
├── BlockBits.hs
├── BlockHead.hs
├── Data
│   ├── Serializable.hs
│   └── Varint.hs
├── ECC.hs
├── Extension
│   ├── ByteString
│   │   ├── Lazy.hs
│   │   └── Parser.hs
│   ├── ByteString.hs
│   └── List.hs
├── FieldElement.hs
├── FiniteFieldEllipticCurve.hs
├── Locktime.hs
├── Network
│   ├── Common.hs
│   └── Messages.hs
├── Network.hs
├── Script
│   ├── PayToPubKeyHash.hs
│   └── PayToScriptHash.hs
├── Script.hs
├── SECP256K1.hs
├── TestnetWalletPassPhrase.hs
├── TestnetWalletPassPhrase.hs.DUMMY
├── Tx.hs
├── TxIn.hs
├── TxOut.hs
└── Utils.hs

5 directories, 25 files
```
