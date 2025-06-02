module GUI (drawBoard) where

import qualified Board

-- drawBoard takes the board and a list of legal moves' destination coordinates
drawBoard :: [[Char]] -> [(Int, Int)] -> IO ()
drawBoard board legalDests = do
    putStrLn "  A B C D E F G H"
    mapM_ (drawRow legalDests) (zip [0..] board)

drawRow :: [(Int, Int)] -> (Int, [Char]) -> IO ()
drawRow legalDests (rowIdx, row) = do
    putStr (show (rowIdx + 1) ++ " ")
    mapM_ (drawCell rowIdx legalDests) (zip [0..] row)
    putStrLn ""

drawCell :: Int -> [(Int, Int)] -> (Int, Char) -> IO ()
drawCell rowIdx legalDests (colIdx, piece) =
    if (rowIdx, colIdx) `elem` legalDests && piece == '.'
    then putStr ("\ESC[32m.\ESC[0m ")  -- green dot for legal moves
    else putStr ([piece] ++ " ")
