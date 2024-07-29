module Input where

import Game
import Moves

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
handleInputs (EventKey (MouseButton LeftButton) Down _ (mX, mY))
    (Game (Board brd) col nMvs prevSel availMoves _) = newGame
    where
        sqSizeInt = round sqSize
        x = round (mX + width/2) `div` sqSizeInt
        y = round (mY + width/2) `div` sqSizeInt

        getOrPlace Nothing brd' = newGame'
            where
                (newSel, newBoard) = replaceNth2d x y (Piece Empty NoColor) brd' []
                newGame' = if pcolor newSel == col
                then Game (Board newBoard) col nMvs (Just newSel) (Just (validMoves newSel (x, y) (Board newBoard))) (mX, mY)
                else Game (Board brd') col nMvs Nothing availMoves (mX, mY)
        getOrPlace (Just piece) brd' = newGame'
            where
                (_, newBoard) = replaceNth2d x y piece brd' []

                avMoves = fromJust availMoves
                newGame' | (x, y) == start (head avMoves) =
                            Game (Board newBoard) col nMvs Nothing Nothing (mX, mY) -- Place back
                         | (x, y) `elem` map end avMoves =
                            Game (Board newBoard) (nextColor col) (nMvs+1) Nothing Nothing (mX, mY) -- Move piece
                         | otherwise = Game (Board brd) col nMvs prevSel availMoves (mX, mY) -- Illegal move

        newGame = getOrPlace prevSel brd


handleInputs (EventMotion (mX, mY)) game = newGame
    where
        (Game (Board brd) col nMvs prevSel availMoves _) = game
        newGame = Game (Board brd) col nMvs prevSel availMoves (mX, mY)
handleInputs _ game = game
