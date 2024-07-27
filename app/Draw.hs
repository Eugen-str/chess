module Draw where

import Game

import Graphics.Gloss

import Data.Map (Map)
import qualified Data.Map as Map

darkPurple,purple,lightPurple,fogWhite,skyBlue,blue,darkBlue,cream,coldWhite :: Color
darkPurple  = makeColorI  69  58  98 255
purple      = makeColorI  94  80 134 255
lightPurple = makeColorI 143  78 139 255
fogWhite    = makeColorI 240 240 240 255
skyBlue     = makeColorI  80 114 167 255
blue        = makeColorI  78  98 114 255
darkBlue    = makeColorI  41  55  69 255
cream       = makeColorI 244 241 226 255
coldWhite   = makeColorI 240 240 250 255

bgColor :: Color
bgColor = fogWhite

showBoard :: [Picture]
showBoard = [pictures [drawSquare x y | let i = y `mod` 2 , x <- [i,i+2..7]] | y <- [0..7] :: [Int]]
    where
        drawSquare x y = color Draw.blue $ translate (topLeft x) (topLeft y) (rectangleSolid sqSize sqSize)

showPiece :: Piece -> Float -> Float -> Map String Picture -> Picture
showPiece (Piece Empty _) _ _ _ = Blank
showPiece p x y assets = translate x y $ scale 0.1 0.1 $ assets Map.! getStrPiece p
    where
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
        showRow (p:xs) x y = showPiece p (topLeft x) (topLeft y) assets : showRow xs (x + 1) y


showGame :: Map String Picture -> Game -> Picture
showGame assets (Game (Board pcs) _ _ sel (mX, mY)) =
    pictures $
        showBoard ++
        showPieces pcs assets ++
        [selPic sel] ++
        showNums
        where
            selPic (Just x) = showPiece x mX mY assets
            selPic Nothing = Blank

            showNums = [translate (-(width/2) + 10) (topLeft y - sqSize / 2.4) $ scale 0.2 0.2 $ (text . show) (8-y) | y <- [7,6..0] :: [Int]]
