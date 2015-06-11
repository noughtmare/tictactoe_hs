import System.IO
import System.Exit

import Data.List
import Data.Function
import Data.Char

import Text.Read

data Player = Player Int | Nobody deriving (Show, Eq, Ord)
data Move = Move (Int, Int) deriving (Show, Eq, Ord)
data Board = Board [[Int]]
instance Show Board where
    show (Board b) = "\n" ++ intercalate "\n-----------\n"  
                     (map ((++) " " . intercalate " | " . (map (token . Player))) b) ++ "\n"

play :: Player -> Move -> Board -> Board
play (Player p) (Move m) (Board b) = Board $ 
    take (fst m) b ++ 
    [(take (snd m) (b !! (fst m)) ++ [p] ++ (drop ((snd m) + 1) (b !! (fst m))))] ++
    (drop ((fst m) + 1) b)

check :: Board -> Player
check (Board b) 
  | [1,1,1] `elem` b ++ (transpose b) = Player 1
  | [2,2,2] `elem` b ++ (transpose b) = Player 2
  | [1,1,1] == (zipWith (!!) b [0,1,2]) || [1,1,1] == (zipWith (!!) b [2,1,0]) = Player 1
  | [2,2,2] == (zipWith (!!) b [0,1,2]) || [2,2,2] == (zipWith (!!) b [2,1,0]) = Player 2
  | foldl1 (*) (map (foldl1 (*)) b) /= 0 = Player 0
  | otherwise = Nobody

inputTable :: [(Int, Int)]
inputTable = [undefined,(2,0),(2,1),(2,2),(1,0),(1,1),(1,2),(0,0),(0,1),(0,2)]

emptyBoard :: Board
emptyBoard = Board [[0,0,0],[0,0,0],[0,0,0]]

other :: Player -> Player
other (Player 1) = Player 2
other (Player 2) = Player 1
other (Player _) = Player 0

color :: Player -> Int
color (Player 2) = -1
color (Player 1) = 1
color (Player 0) = 0

token :: Player -> String
token (Player 1) = "\x1b[31mX\x1b[37m"
token (Player 2) = "\x1b[34mO\x1b[37m"
token _ = " "

possibleMoves :: Board -> [Move]
possibleMoves (Board b) = [Move (y, x) | x <- [0,1,2], y <- [0,1,2], b !! y !! x == 0]

somebody :: Player -> Bool
somebody (Player _) = True
somebody Nobody = False

minimax :: Board -> Player -> Int
minimax (Board b) (Player p)
  | somebody . check . Board $ b = color . check . Board $ b
  | p == 1 = maximum $ map 
                         (\m -> minimax (play (Player p) m (Board b)) (Player 2)) 
                         (possibleMoves (Board b))
  | p == 2 = minimum $ map 
                         (\m -> minimax (play (Player p) m $ Board b) $ Player 1) 
                         $ possibleMoves $ Board b
  | otherwise = error "minimax failed for some reason"

bestMove :: Board -> Player -> Move
bestMove (Board b) (Player p) = 
    maximumBy
        (compare `on` 
            (\m -> (color (Player p)) * (minimax
                (play (Player p) m $ Board b) $ other $ Player p))) $ 
        possibleMoves $ Board b

--------------------------------------------
-- IMPURE CODE
--------------------------------------------

wrongMove :: Board -> Player -> Int -> IO ()
wrongMove (Board b) (Player p) d = do
    putStr $ "\nYou didn't input a valid number,\n" ++
             "please try again: "
    chooseMove (Board b) (Player p) d

chooseMove :: Board -> Player -> Int -> IO ()
chooseMove (Board b) (Player p) d = do
    maybeMove <- fmap readMaybe getLine :: IO (Maybe Int)
    maybe (wrongMove (Board b) (Player p) d)
          (\m -> if (m < 10 && m > 0 && (Move (inputTable !! m)) `elem` (possibleMoves (Board b))) then 
              game (other (Player p)) (play (Player p) (Move (inputTable !! m)) (Board b)) d else
              wrongMove (Board b) (Player p) d)
          maybeMove

endGame :: Player -> IO ()
endGame (Player 1) = do 
    putStrLn "Player 1 is the winner!"
    restartGame
endGame (Player 2) = do 
    putStrLn "Player 2 is the winner!"
    restartGame
endGame (Player 0) = do 
    putStrLn "It's a draw!"
    restartGame
endGame _ = error "endGame failed"

restartGame :: IO ()
restartGame = do
    putStr "Do you want to start another game (Y/N): "
    maybeYes <- fmap (map toUpper) getLine
    if (maybeYes == "Y") then choosePlayer 
    else if (maybeYes == "N") then exitSuccess 
    else restartGame

game :: Player -> Board -> Int -> IO ()
game (Player p) (Board b) d
    | somebody . check . Board $ b = do
        putStrLn $ (show . Board $ b) 
        endGame (check $ Board b)
    | p == 1 = do
        putStrLn $ show $ (Board b)
        putStr $ "Please type the number on your numpad\n" ++ 
                 "that corresponds with the location of\n" ++
                 "the place you want to play: "
        chooseMove (Board b) (Player p) d
    | p == 2 = do
        game (other (Player p))
             (play (Player p) (bestMove (Board b) (Player p)) (Board b))
             d
    | otherwise = error "game failed"

choosePlayer :: IO ()
choosePlayer = do 
    putStr $ "Do you want to be first or second player (1 or 2): "
    maybePlayer <- fmap readMaybe getLine :: IO (Maybe Int)
    maybe (choosePlayer)
          (\p -> if (p == 1 || p == 2) then game (Player p) emptyBoard (0) else choosePlayer)
          (maybePlayer)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to my Tic Tac Toe game!"
    putStrLn "-------------------------------"
    choosePlayer
