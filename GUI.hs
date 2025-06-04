module GUI (drawBoard) where

import qualified Board
import Data.Char (isUpper)

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
drawCell rowIdx legalDests (colIdx, piece)
  | (rowIdx, colIdx) `elem` legalDests && piece == '.' = putStr "\ESC[32m.\ESC[0m "
  | isUpper piece = putStr ("\ESC[31m" ++ [piece] ++ "\ESC[0m ")
  | otherwise = putStr (piece : " ")

