module AI where

import Board (possibleMoves, applyMove)

-- AI simply picks the first available move for black
makeAIMove :: [[Char]] -> IO [[Char]]
makeAIMove board =
    case possibleMoves board 'b' of
        [] -> do
            putStrLn "AI has no valid moves. Game over! \n"
            return board
        (from, to):_ -> do
            putStrLn $ "AI moves: " ++ formatMove (from, to) ++ "\n"
            return (applyMove board from to)

-- Reuse formatting for clarity
formatMove :: ((Int, Int), (Int, Int)) -> String
formatMove ((r1,c1),(r2,c2)) = coordToStr (r1,c1) ++ " -> " ++ coordToStr (r2,c2)

coordToStr :: (Int, Int) -> String
coordToStr (r,c) = toEnum (c + fromEnum 'A') : show (r + 1)
