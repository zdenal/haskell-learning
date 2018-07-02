module Main where

import Lib
import Data
import System.IO
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let game = newGame (fillRandomInBlanks gen grid) languages
  play game

play :: Game -> IO ()
play game = do
  hSetBuffering stdout NoBuffering
  putStrLn "\n\n"
  putStrLn $ formatGame game
  putStrLn "Please enter a word >"
  word <- getLine

  let newGame = playGame game word
      found = score newGame :: Int
      total = totalWords newGame :: Int

  if total == found then
    putStrLn "CONGRATS YOU HAVE FOUND ALL WORDS!!!"
  else
    play newGame
