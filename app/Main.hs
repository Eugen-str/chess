module Main where

import Draw
import Game
import Input

import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Juicy

window :: Display
window = InWindow "chess" (round width, round height) (100, 100)

getAssets :: [String] -> IO [Picture]
getAssets [] = return []
getAssets (x:xs) = do
    curr <- loadJuicyPNG x
    rest <- getAssets xs
    let img = fromJust curr
    return (img : rest)

main :: IO ()
main = do
    let pieceNames = ["bb", "bk", "bn", "bp", "bq", "br", "wb", "wk", "wn", "wp","wq", "wr"]
    assets <- getAssets (map (\x -> "assets/" ++ x ++ ".png") pieceNames)
    let assetMap = Map.fromList (zip pieceNames assets)

    play window bgColor 30 (initGame Menu) (showGame assetMap) handleInputs stepGame
