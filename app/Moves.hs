module Moves where
import Game

getPiece :: Int -> Int -> Board -> Piece
getPiece x y (Board brd) | x < 0 || y < 0 || x > 7 || y > 7 = Piece Empty NoColor
                         | otherwise = brd !! y !! x

validMoves :: Piece -> (Int, Int) -> Board -> [Move]
validMoves (Piece typ col) (a, b) brd =
    let
        back :: Move
        back = Move (a,b) (a,b) in
    back : case typ of
    Pawn -> validPawn col (a,b) brd
    King -> validKing col (a,b) brd
    Queen -> validQueen col (a,b) brd
    Rook -> validRook col (a,b) brd
    Bishop -> validBishop col (a,b) brd
    Knight -> validKnight col (a,b) brd
    Empty -> error "unreachable code"

checkMove :: Int -> Int -> Int -> Int -> Board -> [Move]
checkMove x1 y1 x2 y2 brd =
    [Move (x1,y1) (x2,y2) | ptype (getPiece x2 y2 brd) == Empty, x2 <= 7, x2 >= 0, y2 <= 7, y2 >= 0]

checkTake :: Int -> Int -> Int -> Int -> PlayerColor -> Board -> [Move]
checkTake x1 y1 x2 y2 pc brd =
    [Move (x1,y1) (x2,y2) | pcolor (getPiece x2 y2 brd) == nextColor pc, x2 <= 7, x2 >= 0, y2 <= 7, y2 >= 0]

checkMoveOrTake :: Int -> Int -> Int -> Int -> PlayerColor -> Board -> [Move]
checkMoveOrTake x1 y1 x2 y2 pc brd = if null move then take' else move
    where
        move = checkMove x1 y1 x2 y2 brd
        take' = checkTake x1 y1 x2 y2 pc brd

getDiagonal :: PlayerColor -> (Int, Int) -> Board -> (Int, Int) -> [Move]
getDiagonal curr_player' (x', y') brd' (i, j) | x' > 7 || x' < 0 || y' > 7 || y' < 0 = []
                                              | not (null move) = move ++ getDiagonal curr_player' (x'+i,y'+j) brd' (i, j)
                                              | otherwise = checkTake x' y' (x'+i) (y'+j) curr_player' brd'
            where
                move = checkMove x' y' (x'+i) (y'+j) brd'

validPawn :: PlayerColor -> (Int, Int) -> Board -> [Move]
validPawn curr_player (x, y) brd | y == 1 && curr_player == White = move_fwd ++ move_fwd_2 ++ diag
                                 | y == 6 && curr_player == Black = move_fwd ++ move_fwd_2 ++ diag
                                 | y < 8  = move_fwd ++ diag
                                 | otherwise = []
    where
        mov = if curr_player == White then 1 else -1
        move_fwd = checkMove x y x (y+mov) brd
        move_fwd_2 = checkMove x y x (y+mov*2) brd
        take_right = checkTake x y (x+1) (y+mov) curr_player brd
        take_left = checkTake x y (x-1) (y+mov) curr_player brd
        diag = take_right ++ take_left

validRook :: PlayerColor -> (Int, Int) -> Board -> [Move]
validRook curr_player (x, y) brd = n ++ e ++ w ++ s
    where
        n = getDiagonal curr_player (x, y) brd ( 0, -1)
        e = getDiagonal curr_player (x, y) brd ( 1,  0)
        s = getDiagonal curr_player (x, y) brd ( 0,  1)
        w = getDiagonal curr_player (x, y) brd (-1,  0)

validBishop :: PlayerColor -> (Int, Int) -> Board -> [Move]
validBishop curr_player (x, y) brd = nw ++ ne ++ sw ++ se
    where
        nw = getDiagonal curr_player (x, y) brd (-1, -1)
        sw = getDiagonal curr_player (x, y) brd (-1,  1)
        ne = getDiagonal curr_player (x, y) brd ( 1, -1)
        se = getDiagonal curr_player (x, y) brd ( 1,  1)

validKnight :: PlayerColor -> (Int, Int) -> Board -> [Move]
validKnight curr_player (x, y) brd = a ++ b
    where
        a = concat [checkMoveOrTake x y (x+x2) (y+y2) curr_player brd | x2 <- [-1,1], y2 <- [-2, 2]]
        b = concat [checkMoveOrTake x y (x+x2) (y+y2) curr_player brd | x2 <- [-2,2], y2 <- [-1, 1]]

validQueen :: PlayerColor -> (Int, Int) -> Board -> [Move]
validQueen = (<>) <$> validBishop <*> validRook

validKing :: PlayerColor -> (Int, Int) -> Board -> [Move]
validKing curr_player (x, y) brd = mvs
    where
        mvs = concat [checkMoveOrTake x y (x+x2) (y+y2) curr_player brd
                     | x2 <- [-1..1], y2 <- [-1..1], not (x2 == 0 && y2 == 0)]
