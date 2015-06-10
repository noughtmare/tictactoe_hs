-- TODO: 
-- Make a main loop so that you can start over
-- Fix the question bug

import System.IO
import Data.List
import Data.Maybe
import Data.Function
import Text.Read

data Board = Board [[Int]]
instance Show Board where
    show (Board b) = "\n" ++ intercalate "\n-----------\n"  
                     (map ((++) " " . intercalate " | " . (map (token . Player))) b) ++ "\n"

data Move = Move (Int, Int) deriving (Show, Eq, Ord)

data Player = Player Int deriving (Show)

play :: Player -> Move -> Board -> Board
play (Player p) (Move m) (Board b) = Board $ 
    take (fst m) b ++ 
    [(take (snd m) (b !! (fst m)) ++ [p] ++ (drop ((snd m) + 1) (b !! (fst m))))] ++
    (drop ((fst m) + 1) b)

check :: Board -> Maybe Int
check (Board b) 
  | [1,1,1] `elem` b ++ (transpose b) = Just 1
  | [2,2,2] `elem` b ++ (transpose b) = Just 2
  | [1,1,1] == (zipWith (!!) b [0,1,2]) || [1,1,1] == (zipWith (!!) b [2,1,0]) = Just 1
  | [2,2,2] == (zipWith (!!) b [0,1,2]) || [2,2,2] == (zipWith (!!) b [2,1,0]) = Just 2
  | foldl1 (*) (map (foldl1 (*)) b) /= 0 = Just 0
check _ = Nothing

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
color _ = undefined

token :: Player -> String
token (Player 1) = "X"
token (Player 2) = "O"
token _ = " "

chooseMove :: Board -> Player -> IO ()
chooseMove (Board b) (Player p) = do
    -- TODO: Fix this
    putStr $ "You didn't input a valid number,\n" ++
             "please try again: "
    maybeMove <- fmap readMaybe getLine :: IO (Maybe Int)
    maybe (chooseMove (Board b) (Player p))
          (\m -> if (m < 10 && m > 0 && (Move (inputTable !! m)) `elem` (possibleMoves (Board b))) then 
              game (other (Player p)) (play (Player p) (Move (inputTable !! m)) (Board b)) else
              chooseMove (Board b) (Player p))
          maybeMove

game :: Player -> Board -> IO ()
game (Player p) (Board b)
    | isJust $ check $ Board b = do
        putStrLn $ (show . Board $ b) 
        -- TODO: Fix this, so that is displays human readable text
        putStrLn $ show (check $ Board b) ++ " is the winner!"
    | p == 1 = do
        putStrLn $ show $ (Board b)
        putStr $ "Please type the number on your numpad\n" ++ 
                 "that corresponds with the location of\n" ++
                 "the place you want to play: "
        chooseMove (Board b) (Player p)
    | p == 2 = do
        game (other (Player p))
             (play (Player p) (bestMove (Board b) (Player p)) (Board b))
    | otherwise = undefined

possibleMoves :: Board -> [Move]
possibleMoves (Board b) = [Move (y, x) | x <- [0,1,2], y <- [0,1,2], b !! y !! x == 0]

minimax :: Board -> Player -> Int
minimax (Board b) (Player p)
  | isJust $ check $ Board b = if (fromJust (check (Board b))) == 2 then -1 
                               else if (fromJust (check $ Board b)) == 1 then 1 
                               else 0
  | p == 1 = maximum $ map 
                         (\m -> minimax (play (Player p) m (Board b)) (Player 2)) 
                         (possibleMoves (Board b))
  | p == 2 = minimum $ map 
                         (\m -> minimax (play (Player p) m $ Board b) $ Player 1) 
                         $ possibleMoves $ Board b
  | otherwise = error "minimax failed for some reason"

-- TODO: Make this return [Move]
bestMove :: Board -> Player -> Move
bestMove (Board b) (Player p) = maximumBy 
                                    (compare `on` 
                                        (\m -> (color (Player p)) * (minimax
                                            (play (Player p) m $ Board b) $ other $ Player p))) $ 
                                    possibleMoves $ Board b

choosePlayer :: IO ()
choosePlayer = do 
    putStr $ "Do you want to be first or second player (1 or 2): "
    maybePlayer <- fmap readMaybe getLine :: IO (Maybe Int)
    maybe (choosePlayer)
          (\p -> if (p == 1 || p == 2) then game (Player p) emptyBoard else choosePlayer)
          (maybePlayer)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Welcome to my Tic Tac Toe game!"
    putStrLn "-------------------------------"
    choosePlayer
