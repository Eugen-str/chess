module Input where

import Game

import Graphics.Gloss.Interface.IO.Interact

replaceNth2d :: Int -> Int -> a -> [[a]] -> [[a]] -> (a, [[a]])
replaceNth2d x y new (curr:rest) res | y /= 0 = replaceNth2d x (y - 1) new rest (res ++ [curr])
                                     | y == 0 = let (e, newArr) = replaceNth x new curr [] in
                                        (e, res ++ [newArr] ++ rest)
    where
        replaceNth :: Int -> a -> [a] -> [a] -> (a, [a])
        replaceNth n el (x':xs) res' | n /= 0 = replaceNth (n - 1) el xs (res' ++ [x'])
                                     | n == 0 = (x', res' ++ [el] ++ xs)
        replaceNth _ _ _ _ = error "replaceNth error"
replaceNth2d x y _ _ _ = error $ "replaceNth2d error: x = " <> show x <> " y = " <> show y

handleInputs :: Event -> Game -> Game
handleInputs (EventKey (MouseButton LeftButton) Up _ (mX, mY)) game = newGame
    where
        sqSizeInt = round sqSize
        x = round (mX + width/2) `div` sqSizeInt
        y = round (mY + width/2) `div` sqSizeInt

        (Game (Board brd) nMvs _ _) = game
        (newSel, newBoard) = replaceNth2d x y (Piece Empty NoColor) brd []

        newGame = Game (Board newBoard) (nMvs+1) (Just newSel) (mX, mY)
handleInputs (EventMotion (mX, mY)) game = newGame
    where
        (Game (Board brd) nMvs prevSel _) = game
        newGame = Game (Board brd) nMvs prevSel (mX, mY)
handleInputs _ game = game
