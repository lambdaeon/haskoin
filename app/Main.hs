{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Crypto.Hash             (hashWith, SHA256 (..))
import           Data.ByteString         (ByteString)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Text.IO            as TIO
import           System.IO               (hFlush, stdout)



main :: IO ()
main = do
  putStr "Enter some text: "
  hFlush stdout
  text <- TIO.getLine
  let bs = encodeUtf8 text
  putStrLn $ "You entered: " ++ show bs
  -- let digest = "0x" ++ (show $ hashWith SHA256 bs)
  -- putStrLn $ "SHA256 hash: " ++ show (read digest :: Integer)
  let digest = hashWith SHA256 (hashWith SHA256 bs)
  putStrLn $ "SHA256 hash: " ++ show digest
