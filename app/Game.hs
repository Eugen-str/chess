module Game where

data PieceType = Pawn
               | Rook
               | Knight
               | Bishop
               | Queen
               | King
               | Empty
               deriving Eq

data PlayerColor = White | Black | NoColor deriving Eq

data Piece = Piece { ptype  :: PieceType
                   , pcolor :: PlayerColor}

newtype Board = Board { pieces :: [[Piece]] }

data Game = Game { board         :: Board
                 , player        :: PlayerColor
                 , nMoves        :: Int
                 , selectedPiece :: Maybe Piece
                 , availMoves    :: Maybe [Move]
                 , prevMousePos  :: (Float, Float) }

data Move = Move { start :: (Int, Int)
                 , end   :: (Int, Int) } deriving Eq

width :: Float
width = 800

height :: Float
height = 800

sqSize :: Float
sqSize = width / 8

topLeft :: Int -> Float
topLeft x = fromIntegral x * sqSize - center
    where
        center = width / 2 - sqSize/2

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "ERROR: fromJust - Nothing"

nextColor :: PlayerColor -> PlayerColor
nextColor White = Black
nextColor Black = White
nextColor NoColor = error "ERROR: nextColor"

