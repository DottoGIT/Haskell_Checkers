module Board (parseMove, isValidMove, applyMove, possibleMoves, boardPiece, validJumpsFrom) where

import Data.Char (ord, toUpper, toLower)
import Data.Maybe (isJust, fromJust)
import Data.List (find, nubBy, maximumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

-- Type alias for a jump path and its captured positions
type JumpPath = ([(Int, Int)], [(Int, Int)])

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
        hasJumps = any (\(path, _) -> length path > 1) jumps
    in if hasJumps
       then any (\(path, _) -> head path == start && last path == end && length path > 1) jumps
       else end `elem` simpleMovesFrom board piece start
isValidMove _ _ = False

applyMove :: [[Char]] -> [(Int, Int)] -> [[Char]]
applyMove board [] = board
applyMove board [_] = board
applyMove board [start, end] =
    let piece = boardPiece board start
        allJumps = jumpSequencesFrom board piece start
        maybePath = find (\(path, _) -> head path == start && last path == end && length path > 1) allJumps
    in case maybePath of
        Just (path, captures) ->
            let boardAfterCaptures = foldl removePiece board captures
                boardCleared = setPiece boardAfterCaptures start '.'
                finalPos = last path
                promotedPiece = promoteIfNeeded piece finalPos
            in setPiece boardCleared finalPos promotedPiece
        Nothing ->
            if isJump start end && opponentPiece (boardPiece board (middle start end)) piece
            then let mid = middle start end
                     board' = setPiece (removePiece board mid) start '.'
                 in setPiece board' end piece
            else if end `elem` simpleMovesFrom board piece start
            then let board' = setPiece board start '.'
                     promotedPiece = promoteIfNeeded piece end
                 in setPiece board' end promotedPiece
            else board
applyMove board _ = board

isJump :: (Int, Int) -> (Int, Int) -> Bool
isJump (r1, c1) (r2, c2) = abs (r2 - r1) == 2 && abs (c2 - c1) == 2

-- Recursively find all jump sequences from position, with captured pieces
jumpSequencesFrom :: [[Char]] -> Char -> (Int, Int) -> [JumpPath]
jumpSequencesFrom board piece pos
  | piece == 'W' || piece == 'B' =
      let jumps = validJumpsFrom board piece pos
          allPaths = concatMap (\to -> buildSequences board piece pos to Set.empty) jumps
          maxLen = maximum (0 : map (length . snd) allPaths)
      in filter (\(_, caps) -> length caps == maxLen) allPaths
  | otherwise =
      let jumps = validJumpsFrom board piece pos
          allPaths =
            if null jumps
            then [([pos], [])]
            else do
              nextPos <- jumps
              let jumpedPos = middle pos nextPos
                  boardAfterJump = removePiece board jumpedPos
                  boardCleared = setPiece boardAfterJump pos '.'
                  boardMoved = setPiece boardCleared nextPos piece
              (path, captures) <- jumpSequencesFrom boardMoved piece nextPos
              return (pos : path, jumpedPos : captures)
          maxLen = maximum (0 : map (length . snd) allPaths)
      in filter (\(_, caps) -> length caps == maxLen) allPaths

buildSequences :: [[Char]] -> Char -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> [JumpPath]
buildSequences board piece from to jumped =
  case findJumpedPiece board piece from to of
    Nothing -> []
    Just jumpedPos ->
      if Set.member jumpedPos jumped then [] else
        let jumped' = Set.insert jumpedPos jumped
            boardAfterJump = removePiece board jumpedPos
            boardCleared = setPiece boardAfterJump from '.'
            boardMoved = setPiece boardCleared to piece
            furtherJumps = validJumpsFrom boardMoved piece to
        in if null furtherJumps
           then [([from, to], [jumpedPos])]
           else do
               nextPos <- furtherJumps
               (path, captures) <- buildSequences boardMoved piece to nextPos jumped'
               return (from : path, jumpedPos : captures)

findJumpedPiece :: [[Char]] -> Char -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
findJumpedPiece board player (r1, c1) (r2, c2) =
  let dr = signum (r2 - r1)
      dc = signum (c2 - c1)
      positions = takeWhile (/= (r2, c2)) $ tail $ iterate (\(r, c) -> (r + dr, c + dc)) (r1, c1)
      opponents = filter (\pos -> opponentPiece (boardPiece board pos) player) positions
  in case opponents of
       [pos] -> Just pos
       _     -> Nothing

validJumpsFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
validJumpsFrom board piece pos
  | piece == 'W' || piece == 'B' = concatMap (flyingJumpsInDirection board piece pos) directions
  | piece == 'w' || piece == 'b' = filter canJumpOver immediateJumps
  | otherwise = []
  where
    directions = [(-1,-1), (-1,1), (1,-1), (1,1)]
    (r, c) = pos
    immediateJumps = [ (r + 2*dr, c + 2*dc) | (dr, dc) <- directions, onBoard (r + 2*dr, c + 2*dc) ]
    canJumpOver dest =
      boardPiece board dest == '.' &&
      opponentPiece (boardPiece board (middle pos dest)) piece

flyingJumpsInDirection :: [[Char]] -> Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
flyingJumpsInDirection board player (r, c) (dr, dc) = search (r + dr, c + dc) Nothing
  where
    search pos@(rr, cc) maybeOpponent
      | not (onBoard pos) = []
      | otherwise =
          case boardPiece board pos of
            '.' -> case maybeOpponent of
                     Just _  -> pos : search (rr + dr, cc + dc) maybeOpponent
                     Nothing -> search (rr + dr, cc + dc) maybeOpponent
            p | opponentPiece p player && maybeOpponent == Nothing ->
                  search (rr + dr, cc + dc) (Just pos)
              | otherwise -> []

promoteIfNeeded :: Char -> (Int, Int) -> Char
promoteIfNeeded 'w' (0, _) = 'W'
promoteIfNeeded 'b' (7, _) = 'B'
promoteIfNeeded piece _   = piece

simpleMovesFrom :: [[Char]] -> Char -> (Int, Int) -> [(Int, Int)]
simpleMovesFrom board piece pos =
    filter (\p -> boardPiece board p == '.') (potentialSimpleMoves board pos piece)

-- Get potential simple moves
potentialSimpleMoves :: [[Char]] -> (Int, Int) -> Char -> [(Int, Int)]
potentialSimpleMoves board (r, c) piece
  | piece == 'W' || piece == 'B' = concatMap (slideMoves board (r, c)) directions
  | piece == 'w' = filter onBoard [(r - 1, c - 1), (r - 1, c + 1)]
  | piece == 'b' = filter onBoard [(r + 1, c - 1), (r + 1, c + 1)]
  | otherwise = []
  where
    directions = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
    slideMoves b (rr, cc) (dr, dc) = go (rr + dr, cc + dc)
      where
        go pos@(r2, c2)
          | not (onBoard pos) = []
          | boardPiece b pos == '.' = pos : go (r2 + dr, c2 + dc)
          | otherwise = []

possibleMoves :: [[Char]] -> Char -> [((Int, Int), (Int, Int))]
possibleMoves board player =
    concatMap (\(from, tos) -> map (\to -> (from, to)) tos) (possibleMovesDetailed board player)

possibleMovesDetailed :: [[Char]] -> Char -> [((Int, Int), [(Int, Int)])]
possibleMovesDetailed board player =
    let
        playerPositions = [ (r,c) | r <- [0..7], c <- [0..7], isPlayerPiece (boardPiece board (r,c)) player ]
        allJumpSeqs = [ (pos, jumpSeqs) | pos <- playerPositions
                         , let jumpSeqs = jumpSequencesFrom board (boardPiece board pos) pos
                         , any ((>1) . length . fst) jumpSeqs
                   ]
        maxJumpLen = maximum (0 : concatMap (map (length . fst) . snd) allJumpSeqs)
        filteredJumps = [ (pos, map (last . fst) jumpSeqsFiltered)
                        | (pos, jumpSeqs) <- allJumpSeqs, maxJumpLen > 1
                        , let jumpSeqsFiltered = filter ((== maxJumpLen) . length . fst) jumpSeqs
                        ]
    in if maxJumpLen > 1
       then filteredJumps
       else [ (pos, simpleMovesFrom board (boardPiece board pos) pos) | pos <- playerPositions ]

boardPiece :: [[Char]] -> (Int, Int) -> Char
boardPiece board (r, c) = board !! r !! c

setPiece :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
setPiece board (r,c) piece =
    let row = board !! r
        newRow = replaceAt c piece row
    in replaceAt r newRow board

removePiece :: [[Char]] -> (Int, Int) -> [[Char]]
removePiece board pos = setPiece board pos '.'

middle :: (Int, Int) -> (Int, Int) -> (Int, Int)
middle (r1, c1) (r2, c2) = ((r1 + r2) `div` 2, (c1 + c2) `div` 2)

opponentPiece :: Char -> Char -> Bool
opponentPiece piece player
  | piece == '.' = False
  | toLower player == 'w' = toLower piece == 'b'
  | toLower player == 'b' = toLower piece == 'w'
  | otherwise = False

isPlayerPiece :: Char -> Char -> Bool
isPlayerPiece piece player = toLower piece == toLower player && piece /= '.'

onBoard :: (Int, Int) -> Bool
onBoard (r, c) = r >= 0 && r < 8 && c >= 0 && c < 8

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs
