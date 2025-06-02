module Board (parseMove, isValidMove, applyMove, possibleMoves) where

import Data.Char (ord, toUpper)
import Data.Maybe (isJust, fromJust)
import Data.List (find)

-- Parses input like "A3 B4" or "A3 C5 E3" into list of coordinates
parseMove :: String -> Maybe [(Int, Int)]
parseMove input = mapM parseCoord (words input)

-- Parses single coordinate like "A3" -> (2,0)
parseCoord :: String -> Maybe (Int, Int)
parseCoord [colChar, rowChar]
    | col >= 0 && col < 8 && row >= 0 && row < 8 = Just (row, col)
    | otherwise = Nothing
  where
    col = ord (toUpper colChar) - ord 'A'
    row = ord rowChar - ord '1'
parseCoord _ = Nothing


isValidMove :: [[Char]] -> [(Int, Int)] -> Bool
isValidMove _ [] = False
isValidMove _ [_] = False
isValidMove board [start, end] =
    let piece = boardPiece board start
        jumps = jumpSequencesFrom board piece start
        hasJumps = any (\seq -> length seq > 1) jumps
    in if hasJumps
       then any (\seq -> head seq == start && last seq == end && length seq > 1) jumps
       else end `elem` simpleMovesFrom board piece start



-- Apply multi-step move, removing all jumped pieces between consecutive steps
applyMove :: [[Char]] -> [(Int, Int)] -> [[Char]]
applyMove board [] = board
applyMove board [_] = board
applyMove board [start, end] =
    let piece = boardPiece board start
        allJumps = jumpSequencesFrom board piece start
        maybePath = find (\seq -> head seq == start && last seq == end && length seq > 1) allJumps
    in case maybePath of
        Just (_:rest) -> applyMoveSeq board piece start rest  -- Apply full jump
        Nothing ->
            if isJump start end && opponentPiece (boardPiece board (middle start end)) piece
            then -- valid single jump
                let mid = middle start end
                    board' = setPiece (removePiece board mid) start '.'
                in setPiece board' end piece
            else if end `elem` simpleMovesFrom board piece start
            then -- valid simple move
                let board' = setPiece board start '.'
                in setPiece board' end piece
            else
                board -- invalid move, do nothing


-- Helper to check if a move is a jump (2 cells away diagonally)
isJump :: (Int, Int) -> (Int, Int) -> Bool
isJump (r1, c1) (r2, c2) = abs (r2 - r1) == 2 && abs (c2 - c1) == 2

-- Apply full jump sequence
applyMoveSeq :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)] -> [[Char]]
applyMoveSeq board _ _ [] = board
applyMoveSeq board piece from (to:rest) =
    let mid = middle from to
        board' = setPiece (removePiece board mid) from '.'
        board'' = setPiece board' to piece
    in applyMoveSeq board'' piece to rest



-- Helper to get the piece at a coordinate
boardPiece :: [[Char]] -> (Int, Int) -> Char
boardPiece board (r, c) = board !! r !! c

-- Set a piece on the board at a given position
setPiece :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
setPiece board (r,c) piece =
    let row = board !! r
        newRow = replaceAt c piece row
    in replaceAt r newRow board

-- Remove a piece at given position (set to '.')
removePiece :: [[Char]] -> (Int, Int) -> [[Char]]
removePiece board pos = setPiece board pos '.'

-- Return all possible moves as list of ((from), (to)) pairs for player
possibleMoves :: [[Char]] -> Char -> [((Int, Int), (Int, Int))]
possibleMoves board player =
    concatMap (\(from, tos) -> map (\to -> (from, to)) tos) (possibleMovesDetailed board player)

-- Returns list of (startPos, [endPos]) pairs for player, jumps only if exist
possibleMovesDetailed :: [[Char]] -> Char -> [((Int, Int), [(Int, Int)])]
possibleMovesDetailed board player =
    let
        playerPositions = [ (r,c) | r <- [0..7], c <- [0..7], boardPiece board (r,c) == player ]
        allJumpSeqs = [ (pos, jumpSeqs) | pos <- playerPositions
                         , let jumpSeqs = jumpSequencesFrom board player pos
                         , any ((>1) . length) jumpSeqs
                   ]
        maxJumpLen = maximum (0 : concatMap (map length . snd) allJumpSeqs)
        filteredJumps = [ (pos, map last jumpSeqsFiltered)
                        | (pos, jumpSeqs) <- allJumpSeqs, maxJumpLen > 1
                        , let jumpSeqsFiltered = filter ((== maxJumpLen) . length) jumpSeqs
                        ]
    in if maxJumpLen > 1
       then filteredJumps
       else [ (pos, simpleMovesFrom board player pos) | pos <- playerPositions ]

-- Get simple moves (no jumps) from position
simpleMovesFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
simpleMovesFrom board player pos = 
    filter (\p -> boardPiece board p == '.') (potentialSimpleMoves pos player)

-- Recursively find all jump sequences from position, returns list of sequences of positions
jumpSequencesFrom :: [[Char]] -> Char -> (Int, Int) -> [[(Int, Int)]]
jumpSequencesFrom board player pos =
    let jumps = validJumpsFrom board player pos
    in if null jumps
       then [[pos]]
       else do
         nextPos <- jumps
         let jumpedPos = middle pos nextPos
             boardAfterJump = removePiece board jumpedPos
             boardMoved = setPiece boardAfterJump pos '.' 
             boardMoved' = setPiece boardMoved nextPos player
         rest <- jumpSequencesFrom boardMoved' player nextPos
         return (pos : rest)

-- Valid jumps from a position: landing spots only (not including start pos)
validJumpsFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
validJumpsFrom board player pos = 
    [ dest 
    | dest <- potentialJumpMoves pos player
    , boardPiece board dest == '.'
    , let mid = middle pos dest
    , opponentPiece (boardPiece board mid) player
    ]

-- Simple diagonal moves
potentialSimpleMoves :: (Int, Int) -> Char -> [(Int, Int)]
potentialSimpleMoves (r, c) 'w' = filter onBoard [(r-1, c-1), (r-1, c+1)]
potentialSimpleMoves (r, c) 'b' = filter onBoard [(r+1, c-1), (r+1, c+1)]
potentialSimpleMoves _     _    = []

-- Potential jump moves: two squares diagonally
potentialJumpMoves :: (Int, Int) -> Char -> [(Int, Int)]
potentialJumpMoves (r, c) 'w' = filter onBoard [(r-2, c-2), (r-2, c+2)]
potentialJumpMoves (r, c) 'b' = filter onBoard [(r+2, c-2), (r+2, c+2)]
potentialJumpMoves _     _    = []

-- Gets the coordinate in the middle of a jump
middle :: (Int, Int) -> (Int, Int) -> (Int, Int)
middle (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

-- Checks if a piece belongs to opponent
opponentPiece :: Char -> Char -> Bool
opponentPiece piece player
  | player == 'w' = piece == 'b'
  | player == 'b' = piece == 'w'
  | otherwise     = False

-- Helper: checks if a coordinate is on the board
onBoard :: (Int, Int) -> Bool
onBoard (r, c) = r >= 0 && r < 8 && c >= 0 && c < 8

-- Helper: replaces an element at index in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs
