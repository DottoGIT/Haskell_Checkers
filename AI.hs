module AI where

import qualified Board
import Data.List (maximumBy)

-- Format move for display
formatMove :: ((Int, Int), (Int, Int)) -> String
formatMove ((r1,c1),(r2,c2)) = coordToStr (r1,c1) ++ " -> " ++ coordToStr (r2,c2)

coordToStr :: (Int, Int) -> String
coordToStr (r,c) = toEnum (c + fromEnum 'A') : show (r + 1)

-- AI move using minimax with alpha-beta pruning
makeAIMove :: Int -> [[Char]] -> IO [[Char]]
makeAIMove depth board =
    case Board.possibleMoves board 'b' of
        [] -> do
            putStrLn "AI has no valid moves. Game over! \n"
            return board
        _ -> do
            let move = bestMove board depth
            putStrLn $ "AI moves: " ++ formatMove move ++ "\n"
            return (Board.applyMove board [fst move, snd move])

-- Best move search
bestMove :: [[Char]] -> Int -> ((Int, Int), (Int, Int))
bestMove board depth =
    let moves = Board.possibleMoves board 'b'
        scoredMoves = [ (m, minimax (Board.applyMove board [fst m, snd m]) (depth - 1) (-99999) 99999 False)
                      | m <- moves ]
    in fst $ maximumBy (\(_, s1) (_, s2) -> compare s1 s2) scoredMoves

-- Minimax with alpha-beta pruning
minimax :: [[Char]] -> Int -> Int -> Int -> Bool -> Int
minimax board depth alpha beta isMaximizing
    | depth == 0 || null moves = evaluateBoard board
    | isMaximizing = maxValue moves alpha beta (-99999)
    | otherwise    = minValue moves alpha beta 99999
  where
    player = if isMaximizing then 'b' else 'w'
    moves = Board.possibleMoves board player

    maxValue [] _ _ best = best
    maxValue (m:ms) a b best =
        let newBoard = Board.applyMove board [fst m, snd m]
            val = minimax newBoard (depth - 1) a b False
            newBest = max best val
            newAlpha = max a val
        in if newAlpha >= b then newBest else maxValue ms newAlpha b newBest

    minValue [] _ _ best = best
    minValue (m:ms) a b best =
        let newBoard = Board.applyMove board [fst m, snd m]
            val = minimax newBoard (depth - 1) a b True
            newBest = min best val
            newBeta = min b val
        in if a >= newBeta then newBest else minValue ms a newBeta newBest

-- Simple material-based evaluation
evaluateBoard :: [[Char]] -> Int
evaluateBoard board = sum $ map evaluateCell (concat board)
  where
    evaluateCell :: Char -> Int
    evaluateCell c = case c of
        'b' -> 1
        'B' -> 3
        'w' -> -1
        'W' -> -3
        _   -> 0
