module Moves where
import Game

getPiece :: Int -> Int -> Board -> Piece
getPiece x y (Board brd) | x < 0 || y < 0 || x > 7 || y > 7 = Piece Empty NoColor
                         | otherwise = brd !! y !! x

validMoves :: Piece -> (Int, Int) -> Board -> [Move]
validMoves (Piece Pawn col) a b = validPawn col a b
validMoves _ _ _ = []

checkMove :: Int -> Int -> Int -> Int -> Board -> [Move]
checkMove x1 y1 x2 y2 brd =
    [Move (x1,y1) (x2,y2) | ptype (getPiece x2 y2 brd) == Empty]

checkTake :: Int -> Int -> Int -> Int -> PlayerColor -> Board -> [Move]
checkTake x1 y1 x2 y2 pc brd =
    [Move (x1,y1) (x2,y2) | pcolor (getPiece x2 y2 brd) == nextColor pc]

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
