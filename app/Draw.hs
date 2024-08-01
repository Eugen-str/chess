module Draw where

import Game

import Graphics.Gloss

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (chr, ord)

darkGrey,darkPurple,purple,lightPurple,fogWhite,skyBlue,blue,darkBlue,cream,coldWhite,trPurple,trRed :: Color
darkGrey    = makeColorI  28  28  28 255
darkPurple  = makeColorI  69  58  98 255
purple      = makeColorI  94  80 134 255
lightPurple = makeColorI 143  78 139 255
fogWhite    = makeColorI 240 240 240 255
skyBlue     = makeColorI  80 114 167 255
blue        = makeColorI  78  98 114 255
darkBlue    = makeColorI  41  55  69 255
cream       = makeColorI 244 241 226 255
coldWhite   = makeColorI 240 240 250 255
trPurple    = makeColorI  69  58  98 128
trRed       = makeColorI 158  42  43 180

bgColor :: Color
bgColor = coldWhite

boardWhiteColor :: Color
boardWhiteColor = fogWhite

boardBlackColor :: Color
boardBlackColor = Draw.blue

hgColor :: Color
hgColor = trRed

showBoard :: Color -> [Picture]
showBoard c = [pictures [drawSquare (fromIntegral x*sqSize + sqSize/2 + fromIntegral sx) (fromIntegral y*sqSize + sqSize/2 + fromIntegral sy) c
                        | let i = y `mod` 2 , x <- [i,i+2..7]]
              | y <- [0..7] :: [Int]]
    where
        (sx, sy) = boardLoc

topLeftSq :: Int -> Float
topLeftSq x = fromIntegral x*sqSize + sqSize / 2

drawSquare :: Float -> Float -> Color -> Picture
drawSquare x y c = color c $ translate sx sy (rectangleSolid sqSize sqSize)
    where
        (sx, sy) = topLeftXY (round x, round y)

drawSquareSmall :: Float -> Float -> Color -> Picture
drawSquareSmall x y c = color c $ translate sx sy (rectangleSolid (sqSize - 5) (sqSize - 5))
    where
        (sx, sy) = topLeftXY (round x, round y)

drawCircle :: Int -> Int -> Color -> Picture
drawCircle x y c = color c $ translate cx cy (circleSolid (sqSize / 5))
    where
        (cx, cy) = topLeftXY (x, y)

showPiece :: Piece -> Float -> Float -> Map String Picture -> Picture
showPiece (Piece Empty _) _ _ _ = Blank
showPiece p x y assets = translate x y $ scale pScale pScale $ assets Map.! getStrPiece p
    where
        pScale = sqSize / 1000
        getStrPiece :: Piece -> String
        getStrPiece (Piece ptyp pcol) = res
            where
                col = if pcol == White then "w" else "b"
                typ = case ptyp of
                    Pawn   -> "p"
                    Queen  -> "q"
                    King   -> "k"
                    Rook   -> "r"
                    Bishop -> "b"
                    Knight -> "n"
                    _ -> error "unreachable code"
                res = col ++ typ

showPieces :: [[Piece]] -> Map String Picture -> [Picture]
showPieces pcs assets = showPieces_ pcs 0
    where
        showPieces_ [] _ = []
        showPieces_ (x:xs) row = showRow x 0 row ++ showPieces_ xs (row+1)

        showRow :: [Piece] -> Int -> Int-> [Picture]
        showRow [] _ _ = []
        showRow (p:xs) x y =
            showPiece p (px+fromIntegral bx) (py+fromIntegral by) assets
                : showRow xs (x + 1) y
                    where
                        (px, py) = topLeftXY(round $ topLeftSq x, round $ topLeftSq y)
                        (bx, by) = boardLoc

showGame :: Map String Picture -> Game -> Picture
showGame assets (Game Menu _ _ _ _ _ _ _ _) =
    pictures $
        showTitle :
        showButtonPlay2P ++
        knight
        where
            showTitle = let (x, y) = titlePos in translate x y $ color purple $ text "Chess"

            centerTextInButton x y w h = (x - w / 2 + 20, y - h / 4)

            showButtonPlay2P =
                [translate x y $ color skyBlue $ rectangleWire w h,
                 translate cx cy $ color black $ scale 0.25 0.25 $ text "Play"]
                    where
                        ((x, y), (w, h)) = buttonPlay2P
                        (cx, cy) = centerTextInButton x y w h

            {-
            showButtonQuit =
                [translate x y $ color skyBlue $ rectangleWire w h,
                 translate cx cy $ color black $ scale 0.25 0.25 $ text "Quit"]
                    where
                        ((x, y), (w, h)) = buttonQuit
                        (cx, cy) = centerTextInButton x y w h
            -}

            knight = [scale 5 5 $ showPiece (Piece Rook White) 50 0 assets]

showGame assets (Game _ (Board pcs) _ _ sel moves (mX, mY) bTimer wTimer) =
    pictures $
        boardBgAndBorder ++
        showBoard boardBlackColor ++
        showAvailMoves moves ++
        showPieces pcs assets ++
        [selPic sel] ++
        showNums ++
        showLetters ++
        showTimers
        where
            (bx, by) = boardLoc

            selPic (Just x) = showPiece x mX mY assets
            selPic Nothing = Blank

            boardBgAndBorder = [
                translate xc yc $ color boardWhiteColor $ rectangleSolid a a,
                translate xc yc $ color darkPurple $ rectangleWire a a
                ]
                where
                    a = sqSize*8
                    (x, y) = topLeftXY boardLoc
                    (xc, yc) = (x + a/2, y + a/2)

            showNums =
                [translate (topLeftX $ fromIntegral bx - 20)
                           (topLeftY $ round $ fromIntegral y*sqSize + sqSize / 2.5 + fromIntegral by)
                           $ scale 0.2 0.2 $ (text . show) (8-y)
                | y <- [7,6..0] :: [Int]]
            showLetters =
                [translate (topLeftX $ round $ fromIntegral x*sqSize + sqSize / 3 + fromIntegral bx)
                           (topLeftY $ fromIntegral by - 20)
                           $ scale 0.2 0.2 (text [chr (ord 'a' + x)])
                | x <- [0..7] :: [Int]]

            showAvailMoves (Just mvs) =
                [drawSquareSmall (topLeftSq x + fromIntegral bx) (topLeftSq y + fromIntegral by) hgColor
                | move <- mvs, let (x, y) = end move]
            showAvailMoves Nothing = [Blank]

            showTimers = [translate x1 y $ scale a a $ color black $ (text . showTime) wTimer,
                          translate x2 y $ scale a a $ color black $ (text . showTime) bTimer]
                where
                    x1 = topLeftX bx
                    x2 = topLeftX (round $ 3*width/4)
                    a = 0.3
                    y = height / 2 - 10

                    pad str = if length str == 1 then "0" ++ str else str
                    showTime x = let xi = round x :: Int in (pad . show) (xi `div` 60) ++ ":" ++ (pad . show) (xi `mod` 60)
