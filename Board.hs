module Board (parseMove, isValidMove, applyMove, possibleMoves, boardPiece, validJumpsFrom) where

import Data.Char (ord, toUpper, toLower)
import Data.Maybe (isJust, fromJust)
import Data.List (find)
import Debug.Trace (trace)

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
isValidMove _ _ = False  -- multi-step moves validation can be added if needed

-- Apply multi-step move, removing all jumped pieces between consecutive steps
applyMove :: [[Char]] -> [(Int, Int)] -> [[Char]]
applyMove board [] = board
applyMove board [_] = board
applyMove board [start, end] =
    let piece = boardPiece board start
        allJumps = jumpSequencesFrom board piece start
        maybePath = find (\seq -> head seq == start && last seq == end && length seq > 1) allJumps
    in case maybePath of
        Just (_:rest) -> applyMoveSeq board piece start rest  -- Apply full jump sequence
        Nothing ->
            if isJump start end && opponentPiece (boardPiece board (middle start end)) piece
            then -- valid single jump
                let mid = middle start end
                    board' = setPiece (removePiece board mid) start '.'
                in setPiece board' end piece
            else if end `elem` simpleMovesFrom board piece start
            then -- valid simple move
                let board' = setPiece board start '.'
                    promotedPiece = promoteIfNeeded piece end
                in setPiece board' end promotedPiece
            else board -- invalid move, return unchanged

-- Helper to check if a move is a jump (2 cells away diagonally)
isJump :: (Int, Int) -> (Int, Int) -> Bool
isJump (r1, c1) (r2, c2) = abs (r2 - r1) == 2 && abs (c2 - c1) == 2

-- Apply full jump sequence
applyMoveSeq :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)] -> [[Char]]
applyMoveSeq board _ _ [] = board
applyMoveSeq board piece from (to:rest) =
    let mid = middle from to
        board' = setPiece (removePiece board mid) from '.'
        promotedPiece = promoteIfNeeded piece to
        board'' = setPiece board' to promotedPiece
    in applyMoveSeq board'' promotedPiece to rest

promoteIfNeeded :: Char -> (Int, Int) -> Char
promoteIfNeeded 'w' (0, _) = 'W'  -- white reaches top row
promoteIfNeeded 'b' (7, _) = 'B'  -- black reaches bottom row
promoteIfNeeded piece _   = piece

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
        playerPositions = [ (r,c) | r <- [0..7], c <- [0..7], isPlayerPiece (boardPiece board (r,c)) player ]
        allJumpSeqs = [ (pos, jumpSeqs) | pos <- playerPositions
                         , let jumpSeqs = jumpSequencesFrom board (boardPiece board pos) pos
                         , any ((>1) . length) jumpSeqs
                   ]
        maxJumpLen = maximum (0 : concatMap (map length . snd) allJumpSeqs)
        filteredJumps = [ (pos, map last jumpSeqsFiltered)
                        | (pos, jumpSeqs) <- allJumpSeqs, maxJumpLen > 1
                        , let jumpSeqsFiltered = filter ((== maxJumpLen) . length) jumpSeqs
                        ]
    in if maxJumpLen > 1
       then filteredJumps
       else [ (pos, simpleMovesFrom board (boardPiece board pos) pos) | pos <- playerPositions ]

-- Get simple moves (no jumps) from position
simpleMovesFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
simpleMovesFrom board piece pos = 
    filter (\p -> boardPiece board p == '.') (potentialSimpleMoves board pos piece)

-- Recursively find all jump sequences from position, returns list of sequences of positions
jumpSequencesFrom :: [[Char]] -> Char -> (Int, Int) -> [[(Int, Int)]]
jumpSequencesFrom board piece pos
  | piece == 'W' || piece == 'B' =
      let jumps = validJumpsFrom board piece pos
      in map (\nextPos -> [pos, nextPos]) jumps  -- only single jumps, no recursion
  | otherwise =
      let jumps = validJumpsFrom board piece pos
      in if null jumps
         then [[pos]]
         else do
           nextPos <- jumps
           let jumpedPos = middle pos nextPos
               boardAfterJump = removePiece board jumpedPos
               boardMoved = setPiece boardAfterJump pos '.'
               boardMoved' = setPiece boardMoved nextPos piece
           rest <- jumpSequencesFrom boardMoved' piece nextPos
           return (pos : rest)


-- Valid jumps from a position: sliding jumps only for kings, normal pieces jump two steps
validJumpsFrom board piece pos
  | piece == 'W' || piece == 'B' = concatMap (flyingJumpsInDirection board piece pos) directions
  | piece == 'w' || piece == 'b' = filter canJumpOver immediateJumps
  | otherwise = []
  where
    directions = [(-1,-1), (-1,1), (1,-1), (1,1)]
    (r, c) = pos

    immediateJumps = [ (r+2*dr, c+2*dc) | (dr, dc) <- directions, onBoard (r+2*dr, c+2*dc) ]

    canJumpOver dest =
      boardPiece board dest == '.' &&
      opponentPiece (boardPiece board (middle pos dest)) piece



-- Flying jump for kings: scan diagonally for one opponent piece, then empty squares beyond it

flyingJumpsInDirection :: [[Char]] -> Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
flyingJumpsInDirection board player (r, c) (dr, dc) = search (r + dr, c + dc) Nothing []
  where
    search pos@(rr, cc) maybeOpponent foundSquares
      | not (onBoard pos) = []
      | otherwise =
          case boardPiece board pos of
            '.' ->
              case maybeOpponent of
                Just _  -> pos : search (rr + dr, cc + dc) maybeOpponent (pos : foundSquares)
                Nothing -> search (rr + dr, cc + dc) maybeOpponent foundSquares
            pieceAtPos
              | opponentPiece pieceAtPos player && maybeOpponent == Nothing -> search (rr + dr, cc + dc) (Just pos) foundSquares
              | otherwise -> []




-- Simple diagonal moves: kings slide any number of squares; men only one step forward
potentialSimpleMoves :: [[Char]] -> (Int, Int) -> Char -> [(Int, Int)]
potentialSimpleMoves board (r, c) piece
  | piece == 'W' || piece == 'B' = concatMap (slideMoves board (r, c)) directions
  | piece == 'w' = filter onBoard [(r-1, c-1), (r-1, c+1)]
  | piece == 'b' = filter onBoard [(r+1, c-1), (r+1, c+1)]
  | otherwise = []
  where
    directions = [(-1,-1), (-1,1), (1,-1), (1,1)]

    slideMoves :: [[Char]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
    slideMoves b (rr, cc) (dr, dc) = go (rr + dr, cc + dc)
      where
        go pos@(r2, c2)
          | not (onBoard pos) = []
          | boardPiece b pos == '.' = pos : go (r2 + dr, c2 + dc)
          | otherwise = []

-- Gets the coordinate in the middle of a jump
middle :: (Int, Int) -> (Int, Int) -> (Int, Int)
middle (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

-- Checks if a piece belongs to opponent
opponentPiece :: Char -> Char -> Bool
opponentPiece piece player
  | piece == '.' = False
  | toLower player == 'w' = toLower piece == 'b'
  | toLower player == 'b' = toLower piece == 'w'
  | otherwise = False

-- Checks if a piece belongs to the player (case-insensitive)
isPlayerPiece :: Char -> Char -> Bool
isPlayerPiece piece player
  | toLower piece == toLower player && piece /= '.' = True
  | otherwise = False

-- Helper: checks if a coordinate is on the board
onBoard :: (Int, Int) -> Bool
onBoard (r, c) = r >= 0 && r < 8 && c >= 0 && c < 8

-- Helper: replaces an element at index in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs
