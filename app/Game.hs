module Game where

data GameState = Menu
               | Playing
               | WhiteVictory
               | BlackVictory
               deriving Eq

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

data Game = Game { gameState     :: GameState
                 , board         :: Board
                 , player        :: PlayerColor
                 , nMoves        :: Int
                 , selectedPiece :: Maybe Piece
                 , availMoves    :: Maybe [Move]
                 , prevMousePos  :: (Float, Float)
                 , blackTimer    :: Float
                 , whiteTimer    :: Float }

data Move = Move { start :: (Int, Int)
                 , end   :: (Int, Int) } deriving Eq

width :: Float
width = 1000

height :: Float
height = 800

sqSize :: Float
sqSize = height / 9

boardLoc :: (Int, Int)
boardLoc = ( round $ (width  - sqSize * 8) / 2
           , round $ (height - sqSize * 8) / 2)

sqSizeI :: Int
sqSizeI = round sqSize

topLeftX :: Int -> Float
topLeftX x = fromIntegral x - centerX
    where
        centerX = width / 2

topLeftY :: Int -> Float
topLeftY y = fromIntegral y - centerY
    where
        centerY = height / 2

topLeftXY :: (Int, Int) -> (Float, Float)
topLeftXY (x,y) = (fromIntegral x - centerX, fromIntegral y - centerY)
    where
        centerX = width / 2
        centerY = height / 2

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "ERROR: fromJust - Nothing"

nextColor :: PlayerColor -> PlayerColor
nextColor White = Black
nextColor Black = White
nextColor NoColor = error "ERROR: nextColor"
