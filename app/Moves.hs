module Moves where
import Game

validMoves :: Piece -> (Int, Int) -> Board -> [Move]
validMoves (Piece Pawn col) a b = validPawn col a b
validMoves _ _ _ = []

validPawn :: PlayerColor -> (Int, Int) -> Board -> [Move]
validPawn White (x, y) brd = [Move (x, y) (x, y+1), Move (x, y) (x, y+2)]
validPawn Black (x, y) brd = [Move (x, y) (x, y-1)]
