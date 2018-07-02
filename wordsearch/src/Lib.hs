module Lib
    ( findWord
    , findWords
    , findWordInLine
    , formatGrid
    , gridWithCoords
    , coordsGrid
    , zipOverGridWith
    , Cell(..)
    , cell2char
    , newGame
    , Game(..)
    , playGame
    , formatGame
    , score
    , totalWords
    , fillRandomInBlanks
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes, listToMaybe)
import System.Random
import Data.Char
import qualified Data.Map as M

type Grid a = [[a]]

data Cell = Indent | Cell (Integer, Integer) Char deriving (Ord, Show, Eq)

data Game = Game {
    getGrid :: Grid Cell
  , getWords :: M.Map String (Maybe [Cell])
} deriving (Show)

newGame :: Grid Char -> [String] -> Game
newGame grid words =
  let gwc = gridWithCoords grid
      wordDefault word = (word, Nothing)
      wordList = M.fromList $ map wordDefault words
  in Game gwc wordList

totalWords :: Game -> Int
totalWords game = length . M.keys $ getWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ getWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (getWords game) = game
playGame game@(Game grid words) word =
  let found = findWord grid word
  in case found of
    Nothing -> game
    Just _ -> game { getWords = M.insert word found words }

formatGame :: Game -> String
formatGame game = formatGameGrid game
                    ++ "\n\n"
                    ++ (show $ score game )
                    ++ " / "
                    ++ (show $ totalWords game )
                    ++ "\n\n"

coordsGrid =
  let columns = repeat [0..]
      rows = map repeat [0..]
  in zipOverGrid rows columns

zipOverGrid = zipWith zip
zipOverGridWith = zipWith . zipWith

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords = zipOverGridWith Cell coordsGrid

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

formatGameGrid :: Game -> String
formatGameGrid (Game grid words) =
  let foundWords = concat . catMaybes . M.elems $ words
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` foundWords then char else toLower char
  in unlines $ mapOverGrid formatCell grid


formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char Indent = '?'
cell2char (Cell _ c) = c

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = catMaybes $ map (findWord grid) words

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
     Nothing -> findWordInLine word (tail line)
     cs@(Just _) -> cs

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word =
  let lines = getLines grid
      found = map (findWordInLine word) lines
  in listToMaybe $ catMaybes found

makeRandomGrid gen =
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen
  in row : makeRandomGrid gen2

fillRandomInBlanks gen grid =
  let randomGrid = makeRandomGrid gen
      fill '_' c = c
      fill c _ = c
  in zipOverGridWith fill grid randomGrid

getLines :: Grid Cell -> [[Cell]]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ (map reverse lines)

diagonalize :: Grid Cell -> Grid Cell
diagonalize = transpose . skew

skew :: Grid Cell -> Grid Cell
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = Indent : line

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs)
  | x == cell2char c = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing
