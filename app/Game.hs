{-# LANGUAGE RecordWildCards #-}

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

-- Definitions for the menu buttons / title

-- (x + w / 2) (y + h / 4)
--
titlePos :: (Float, Float)
titlePos = (topLeftX 50, height/2 - 50)

buttonPlay2P :: ((Float, Float), (Float, Float))
buttonPlay2P = let (x, y) = titlePos in ((x + w/2, y + h / 4 - 200), (w, h))
    where
        w = 200
        h = 50

buttonQuit :: ((Float, Float), (Float, Float))
buttonQuit = let (x, _) = fst buttonPlay2P in ((x, topLeftY 50), (200, 50))

--

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

makeBoard :: [String] -> Board
makeBoard strs = Board (makeBoard_ strs)
    where
    makeBoard_ :: [String] -> [[Piece]]
    makeBoard_ [] = []
    makeBoard_ [x] = [map charToPiece x]
    makeBoard_ (x:xs) = map charToPiece x : makeBoard_ xs

    charToPiece :: Char -> Piece
    charToPiece x = case x of
        'q' -> Piece Queen Black
        'Q' -> Piece Queen White
        'p' -> Piece Pawn Black
        'P' -> Piece Pawn White
        'n' -> Piece Knight Black
        'N' -> Piece Knight White
        'k' -> Piece King Black
        'K' -> Piece King White
        'r' -> Piece Rook Black
        'R' -> Piece Rook White
        'b' -> Piece Bishop Black
        'B' -> Piece Bishop White
        ' ' -> Piece Empty NoColor
        _ -> error $ "Unkown piece: " <> [x]

initGame :: GameState -> Game
initGame s = Game s b White 0 Nothing Nothing (0, 0) (60*5) (60*5)
    where
        b = makeBoard [
            "RNBQKBNR",
            "PPPPPPPP",
            "        ",
            "        ",
            "        ",
            "        ",
            "pppppppp",
            "rnbqkbnr"
            ]

stepGame :: Float -> Game -> Game
stepGame time (Game {..}) = Game {whiteTimer = if player == White && gameState == Playing then whiteTimer - time else whiteTimer,
                                  blackTimer = if player == Black && gameState == Playing then blackTimer - time else blackTimer,
                                  gameState  = if whiteTimer <= 0 && gameState == Playing then BlackVictory
                                          else if blackTimer <= 0 && gameState == Playing then WhiteVictory else gameState,
                                  ..}
