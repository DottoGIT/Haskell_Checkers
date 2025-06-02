module Board (parseMove, isValidMove, applyMove, possibleMoves) where

import Data.Char (ord, toUpper)

-- Parses input like "A3 B4" into coordinate pairs ((row1,col1),(row2,col2))
-- We will adjust this to parse multi-step jumps: e.g. "A3 C5 E3"
parseMove :: String -> Maybe ((Int, Int), (Int, Int))
parseMove input = 
    case words input of
        [from, to] -> do
            fromCoord <- parseCoord from
            toCoord   <- parseCoord to
            return (fromCoord, toCoord)
        _ -> Nothing

-- Parses single coordinate like "A3" -> (2,0)
parseCoord :: String -> Maybe (Int, Int)
parseCoord [colChar, rowChar]
    | col >= 0 && col < 8 && row >= 0 && row < 8 = Just (row, col)
    | otherwise = Nothing
  where
    col = ord (toUpper colChar) - ord 'A'
    row = ord rowChar - ord '1'
parseCoord _ = Nothing

-- Checks whether a move is valid: must be one of the possible moves (including multi-jumps)
isValidMove :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board from to =
    let piece = boardPiece board from
        moves = concatMap (\(f, tos) -> map (\t -> (f, t)) tos) (possibleMovesDetailed board piece)
    in (from, to) `elem` moves


-- Applies a move (single step or jump), removes all jumped pieces if any
applyMove :: [[Char]] -> (Int, Int) -> (Int, Int) -> [[Char]]
applyMove board from to = 
    let piece = boardPiece board from
        jumpedPos = if abs (fst to - fst from) == 2 && abs (snd to - snd from) == 2
                    then Just ((fst from + fst to) `div` 2, (snd from + snd to) `div` 2)
                    else Nothing
        boardAfterJump = case jumpedPos of
            Just pos -> removePiece board pos
            Nothing  -> board
        boardClearedFrom = setPiece boardAfterJump from '.'
        boardPlacedTo = setPiece boardClearedFrom to piece
    in boardPlacedTo

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
-- Enforces jump priority: if any jump exists, return only jumps, else simple moves
possibleMoves :: [[Char]] -> Char -> [((Int, Int), (Int, Int))]
possibleMoves board player =
    concatMap (\(from, tos) -> map (\to -> (from, to)) tos) (possibleMovesDetailed board player)


-- Returns list of (startPos, [endPos]) pairs for player, jumps only if exist
possibleMovesDetailed :: [[Char]] -> Char -> [((Int, Int), [(Int, Int)])]
possibleMovesDetailed board player =
    let playerPositions = [ (r,c) | r <- [0..7], c <- [0..7], boardPiece board (r,c) == player ]
        allJumps = [ (pos, jumpDests) | pos <- playerPositions
                   , let jumpDests = concatMap tail (jumpSequencesFrom board player pos)
                   , not (null jumpDests)
                   ]
    in if not (null allJumps)
       then allJumps
       else
           [ (pos, simpleMovesFrom board player pos) | pos <- playerPositions ]

-- Get simple moves (no jumps) from position
simpleMovesFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
simpleMovesFrom board player pos = 
    filter (\p -> boardPiece board p == '.') (potentialSimpleMoves pos player)

-- Recursively find all jump sequences from position, returns list of sequences of positions
jumpSequencesFrom :: [[Char]] -> Char -> (Int, Int) -> [[(Int, Int)]]
jumpSequencesFrom board player pos =
    let jumps = validJumpsFrom board player pos
    in if null jumps
       then [[pos]]  -- no more jumps, chain ends here
       else do
         nextPos <- jumps
         -- Apply the jump to get new board
         let jumpedPos = middle pos nextPos
             boardAfterJump = removePiece board jumpedPos
             boardMoved = setPiece boardAfterJump pos '.' -- remove from current
             boardMoved' = setPiece boardMoved nextPos player -- place at next pos
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
