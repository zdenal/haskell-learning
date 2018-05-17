{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import Data.Monoid
import Data.Time
import System.IO
import System.Console.ANSI
import Control.Exception
import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B
import qualified Data.List as L

data Block = Block {
    index :: Int
  , timestamp :: String
  , prevHash :: String
  , puzzle :: String
  , transactions :: String
  , nonce :: Int
  , hash :: String
} deriving (Show)

type BlockChain = [Block]

genHash :: Int -> String -> String -> String -> Int -> String -> String
genHash index timestamp prevHash puzzle nonce transactions =
  let string = mconcat [show index, timestamp, prevHash, puzzle, show nonce, transactions]
  in C.unpack $ B.encode $ SHA256.hash $ C.pack string

pow :: Int -> String -> String -> String -> Int -> String -> (Int, String)
pow index timestamp prevHash puzzle nonce transactions
  | L.isPrefixOf puzzle hash = (nonce, hash)
  | otherwise = pow index timestamp prevHash puzzle (nonce + 1) transactions
  where hash = genHash index timestamp prevHash puzzle nonce transactions


createBlock :: Int -> String -> String -> String -> String -> Block
createBlock index time prevHash puzzle transactions =
  let (nonce, hash) = pow index time prevHash puzzle 0 transactions
  in Block index time prevHash puzzle transactions nonce hash

mineBlock :: BlockChain -> String -> String -> String -> BlockChain
mineBlock [] time transactions puzzle = (createBlock 0 time "" puzzle transactions) : []
mineBlock chain@(prev:_) time transactions puzzle =
  let prevHash = hash prev
      index = length chain
      block = createBlock index time prevHash puzzle transactions

  in block : chain

getTimeS :: IO (String)
getTimeS = do
  time <- getCurrentTime
  return $ show time

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  clearScreen
  setCursorPosition 0 0
  askForBlock []

askForBlock :: BlockChain -> IO ()
askForBlock chain = do
  putStrLn "Please enter a puzzle or put `q` for quit >"
  puzzle <- getLine

  putStrLn "Please enter a transactions >"
  transactions <- getLine

  clearScreen
  setCursorPosition 0 0

  time <- getTimeS

  start <- getCurrentTime
  let chain' = mineBlock chain time transactions puzzle
      (current:_) = chain'
  putStrLn $ "New block generated with hash: " ++ hash current
  end <- getCurrentTime
  putStrLn $ "Eval time: " ++ (show $ diffUTCTime end start)

  putStrLn "BlockChain is:"
  print chain'
  putStrLn "\n"
  askForBlock chain'
