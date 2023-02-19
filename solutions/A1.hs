module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char  -> Int
convertRowIndex x = (fromEnum (toUpper x)) -65

-- Q#04
_INVALID_MOVE_ :: (Int,Int)
_INVALID_MOVE_ = (-1,-1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | Void  deriving (Show, Eq)



-- Q#07
data GameState = Xwon | Owon | Tie | InProgress  deriving (Show, Eq)


-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int,Int)






-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer s = if s 
    then X
    else O


getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ s 
    | s==True = X
    | s==False = O

-- Q#10
showGameState :: GameState -> [Char]
showGameState gs = case gs of
    Xwon        -> "X has won"
    Owon        -> "O has won"
    Tie         -> "Tie"
    InProgress  -> "The game is in progress"

-- Q#11
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer _  = Void


-- Q#12
showSquare :: Square -> [Char]
showSquare s
    | s == X = "X"
    | s == O = "O"
    | s == Void = "_"