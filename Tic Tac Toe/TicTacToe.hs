module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

-- gameOver :: Board -> Bool
gameOver board = or $ map checkCells [rows board, diags board, cols board]
  where 
    checkCells cells = or $ map isWin (map nub cells)
    isWin (Taken player : []) = True
    isWin _                   = False
-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition str = getMaybePosition $ parseWhite str 0
    where
      parseWhite "" _ = (Nothing, Nothing)
      parseWhite (c:xs_str) n
        | c == ' '    = (readMaybe (take n str) :: Maybe Int, readMaybe xs_str :: Maybe Int) 
        | otherwise   = parseWhite xs_str (n+1)
      
      getMaybePosition (Just a, Just b) = Just (a, b)
      getMaybePosition (a, b)
        | a == Nothing || b == Nothing  = Nothing

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (x,y) board@(b,n)
  | x < 0 || y < 0 || x >= n || y >= n = Nothing
  | checkBoard $ b !! pos_board            = Nothing
  | otherwise                              = Just (new_board,n)
  where
    pos_board             = x + (y * n) 
    new_board             = take pos_board b ++ [Taken player] ++ drop (pos_board+1) b
    checkBoard (Taken _)  = True    
    checkBoard (Empty)    = False    

-------------------------------------------------------------------
-- I/O Functions
prettyPrint :: Board -> IO ()
prettyPrint board = putStrLn "" >> printRows (rows board)
  where
    printRows (row:xs_rows) =  putStrLn (intersperse '|' $ intersperse ' ' $ concat $ map (getCellPretty) row) >> printRows xs_rows
    printRows _             =  putStrLn ""
    getCellPretty (Taken x) =  show x
    getCellPretty Empty     =  "-" 

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).

takeTurn :: Board -> Player -> IO Board
takeTurn board player = 
  do 
    pos <- doParseAction (parsePosition) 
    let modified_board = tryMove player pos board
    isModified modified_board 
  where 
    isModified (Just b)  = return b
    isModified (Nothing) = 
      do 
        putStr "Invalid Move made. Try Again : "
        takeTurn board player

doParseAction :: (String -> Maybe a) -> IO a  
doParseAction f =
    do 
      inp <- getLine 
      checkError (f inp)
  where
    checkError (Just x)  = return x 
    checkError (Nothing) = 
      do 
        putStr "Invalid Input. Try Again : " 
        doParseAction f 
        
        
  

    

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player = 
  do 
    prettyPrint board
    putStr ("Player " ++ (show player) ++ ", make your move (row col) : ")
    modified_board <- takeTurn board player
    prettyPrint modified_board
    if gameOver modified_board && player == X
      then putStrLn  "X is the Winner !!!"      
    else if gameOver modified_board && player == O
      then putStrLn  "O is the Winner !!!"
    else if player == O 
      then playGame modified_board X 
    else playGame modified_board O

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = 
  do
    putStrLn  "Welcome to TicTacToe, This gonna be epic for sho"
    putStrLn  "Da Board has dimensions of N x N "
    putStr    "Enter Board Dimensions(N) : "
    n <- doParseAction getDims
    let board = take (n^2) (repeat Empty)
    playGame  (board,n) X
    putStrLn   "Thanks for playing G, u a real on. Always keep it 100"
  where
    getDims board_dim = (readMaybe board_dim :: Maybe Int)

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
