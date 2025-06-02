module Main where

import qualified GUI
import qualified Board
import qualified AI

initialBoard :: [[Char]]
initialBoard =
    [ ['.', 'b', '.', 'b', '.', 'b', '.', 'b']
    , ['b', '.', 'b', '.', 'b', '.', 'b', '.']
    , ['.', 'b', '.', 'b', '.', 'b', '.', 'b']
    , ['.', '.', '.', '.', '.', '.', '.', '.']
    , ['.', '.', '.', '.', '.', '.', '.', '.']
    , ['w', '.', 'w', '.', 'w', '.', 'w', '.']
    , ['.', 'w', '.', 'w', '.', 'w', '.', 'w']
    , ['w', '.', 'w', '.', 'w', '.', 'w', '.']
    ]

---------------------------------- PRINTING ----------------------------------
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

printMoves :: [[Char]] -> IO ()
printMoves board = do
    let moves = Board.possibleMoves board 'w'
    mapM_ (putStrLn . formatMove) moves

formatMove :: ((Int, Int), (Int, Int)) -> String
formatMove ((r1,c1),(r2,c2)) = coordToStr (r1,c1) ++ " -> " ++ coordToStr (r2,c2)

coordToStr :: (Int, Int) -> String
coordToStr (r,c) = toEnum (c + fromEnum 'A') : show (r + 1)


---------------------------------- GAME LOGIC ----------------------------------

gameLoop :: [[Char]] -> IO ()
gameLoop board = do
    let legalDestinations = map snd (Board.possibleMoves board 'w')
    GUI.drawBoard board legalDestinations

    putStrLn "\nAvaiable Moves: "
    printMoves board
    putStrLn "\nEnter move (e.g. A3 B4):"
    input <- getLine
    let maybeMove = Board.parseMove input
    case maybeMove of
        Nothing -> do
            clearScreen
            putStrLn "Invalid input, try again."
            gameLoop board
        Just positions ->
            if Board.isValidMove board positions
            then do
                clearScreen
                let newBoard = Board.applyMove board positions
                newBoardAfterAIMove <- AI.makeAIMove newBoard
                gameLoop newBoardAfterAIMove
            else do
                clearScreen
                putStrLn "Invalid move, try again."
                gameLoop board


main :: IO ()
main = do 
    clearScreen
    gameLoop initialBoard
