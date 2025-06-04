module Main where

import qualified GUI
import qualified Board
import qualified AI
import System.IO (hFlush, stdout)

initialBoard :: [[Char]]
initialBoard =
    [ ['.', 'b', '.', 'b', '.', 'b', '.', 'b']  -- Row 1
    , ['b', '.', 'b', '.', 'b', '.', 'b', '.']  -- Row 2
    , ['.', 'b', '.', 'b', '.', 'b', '.', 'b']  -- Row 3
    , ['.', '.', '.', '.', '.', '.', '.', '.']  -- Row 4
    , ['.', '.', '.', '.', '.', '.', '.', '.']  -- Row 5
    , ['w', '.', 'w', '.', 'w', '.', 'w', '.']  -- Row 6
    , ['.', 'w', '.', 'w', '.', 'w', '.', 'w']  -- Row 7
    , ['w', '.', 'w', '.', 'w', '.', 'w', '.']  -- Row 8
    ]

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

-- GAME LOOP with AI depth parameter
gameLoop :: Int -> [[Char]] -> IO ()
gameLoop depth board = do
    -- Check if player (white) has moves
    let playerMoves = Board.possibleMoves board 'w'
    if null playerMoves
        then do
            clearScreen
            putStrLn "No moves left for White (Player)."
            putStrLn "Black (AI) wins! Game Over."
        else do
            let legalDestinations = map snd playerMoves
            GUI.drawBoard board legalDestinations
            putStrLn "\nAvailable Moves: "
            printMoves board
            putStrLn "\nEnter move (e.g. A3 B4):"
            input <- getLine
            let maybeMove = Board.parseMove input
            case maybeMove of
                Nothing -> do
                    clearScreen
                    putStrLn "Invalid input, try again."
                    gameLoop depth board
                Just positions ->
                    if Board.isValidMove board positions
                    then do
                        clearScreen
                        let newBoard = Board.applyMove board positions
                        -- Check if AI has moves
                        let aiMoves = Board.possibleMoves newBoard 'b'
                        if null aiMoves
                            then do
                                GUI.drawBoard newBoard []
                                putStrLn "No moves left for Black (AI)."
                                putStrLn "White (Player) wins! Game Over."
                            else do
                                newBoardAfterAIMove <- AI.makeAIMove depth newBoard
                                gameLoop depth newBoardAfterAIMove
                    else do
                        clearScreen
                        putStrLn "Invalid move, try again."
                        gameLoop depth board

-- Entry screen for difficulty selection
chooseDifficulty :: IO Int
chooseDifficulty = do
    putStrLn "Welcome to Minimax Haskell Checkers!"
    putStrLn "Author: Maciej Scheffer \n"
    putStrLn "Choose difficulty level:"
    putStrLn "1) Easy (depth 2)"
    putStrLn "2) Medium (depth 3)"
    putStrLn "3) Hard (depth 5)"
    putStrLn "4) Extreme (depth 8)"
    putStr "Enter level number: "
    hFlush stdout
    levelStr <- getLine
    let depth = case levelStr of
                    "1" -> 2
                    "2" -> 3
                    "3" -> 5
                    "4" -> 8
                    _   -> 3  -- default medium
    putStrLn $ "Starting game at depth " ++ show depth ++ "...\n"
    return depth

main :: IO ()
main = do
    clearScreen
    depth <- chooseDifficulty
    clearScreen
    gameLoop depth initialBoard
