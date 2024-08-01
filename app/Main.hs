{-# LANGUAGE RecordWildCards #-}

module Main where

import Draw
import Game
import Input

import qualified Data.Map as Map

import Graphics.Gloss

import Graphics.Gloss.Juicy

window :: Display
window = InWindow "chess" (round width, round width) (100, 100)

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

initGame :: Game
initGame = Game Playing b White 0 Nothing Nothing (0, 0) (60*5) (60*5)
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

getAssets :: [String] -> IO [Picture]
getAssets [] = return []
getAssets (x:xs) = do
    curr <- loadJuicyPNG x
    rest <- getAssets xs
    let img = fromJust curr
    return (img : rest)

main :: IO ()
main = do
    let pieceName = ["bb", "bk", "bn", "bp", "bq", "br", "wb", "wk", "wn", "wp","wq", "wr"]
    assets <- getAssets (map (\x -> "assets/" ++ x ++ ".png") pieceName)
    let assetMap = Map.fromList (zip pieceName assets)

    play window bgColor 30 initGame (showGame assetMap) handleInputs stepGame
