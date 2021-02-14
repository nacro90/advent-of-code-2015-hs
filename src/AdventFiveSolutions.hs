module AdventFiveSolutions where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as Set

parseInt :: String -> Int
parseInt str = read str :: Int

-- Day 1
numerateParen :: Char -> Int
numerateParen '(' = 1
numerateParen ')' = -1

notQuiteLisp :: String -> Int
notQuiteLisp parenStr = sum $ map numerateParen parenStr

notQuiteLispPartTwo :: String -> Int
notQuiteLispPartTwo parenStr = fromMaybe 0 basementIndex + 1
  where
    basementIndex = findIndex (< 0) $ scanl1 (+) $ map numerateParen parenStr

-- Day 2
paperSurface :: Int -> Int -> Int -> Int
paperSurface l w h = 2 * wl + 2 * wh + 2 * hl + slack
  where
    wl = w * l
    wh = w * h
    hl = h * l
    slack = min wl $ min wh hl

paperSurfaceDim :: Dimensions -> Int
paperSurfaceDim (Dimensions l w h) = paperSurface l w h

data Dimensions =
  Dimensions Int Int Int
  deriving (Show, Eq)

parseDimLine :: String -> Dimensions
parseDimLine line = Dimensions (head elems) (elems !! 1) (elems !! 2)
  where
    elems = map parseInt $ splitOn "x" line

sumPaperSurface :: String -> Int
sumPaperSurface dimsText =
  sum $ paperSurfaceDim . parseDimLine <$> splitOn "\n" dimsText

ribbon :: Dimensions -> Int
ribbon (Dimensions len wid he) = smallestPerimeter + len * wid * he
  where
    smallestPerimeter = sum $ map (* 2) $ filter (/= max_dim) [len, wid, he]
    max_dim = max len $ max wid he

sumRibbon :: String -> Int
sumRibbon dimsText = sum $ ribbon . parseDimLine <$> splitOn "\n" dimsText

-- Day 3
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)

uniqueCoordinates :: String -> Int
uniqueCoordinates str = length stepSet
  where
    stepSet = Set.fromList coordinateSteps
    coordinateSteps = scanl move (0, 0) str
