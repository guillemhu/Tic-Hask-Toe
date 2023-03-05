{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import GHC.Num (bitInteger)
import Control.Concurrent (yield)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> [Char] 
promptPlayer x = "Player " ++ show X ++ "'s turn: enter a row and column position (ex. A1)"

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. _SIZE_-1]

-- Q#03
isDigit :: Char -> Bool
isDigit c =  elem c ['0' .. '9']

readDigit :: Char -> Int
readDigit c  
    | isDigit c = read [c]
    | otherwise = -1

-- Q#04
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Void

_EMPTY_BOARD_ :: [Row]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = elem Void (concat b) == False

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]



-- Q#06
aux :: [Char] -> [String] -> [(Char,String)]
aux xs [] = []
aux [] xs = []
aux (x:xs) (y:ys) = (x,y) : aux xs ys
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings = aux ['A','B' ..]

-- Q#07
formatLine :: [String] -> String
formatLine xs = _SEP_ ++ (intercalate _SEP_ xs) ++ _SEP_
    

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x,y) = all (== True) [(x>=0)&&(x<_SIZE_),(y>=0)&&(y<_SIZE_)] 

-- Q#09
stringToMove :: String -> Move
stringToMove xs
    | length xs == 2 = (convertRowIndex (head xs), readDigit (last  xs))
    | otherwise = (-1,-1)

-- Q#10

replaceSquareInRow = undefined