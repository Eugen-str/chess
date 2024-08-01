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
    oldGame@(Game state (Board brd) col nMvs prevSel availMvs _ bt wt) =
        if clickedWithinBoard then newGame else oldGame
    where
        (tempAx, tempAy) = topLeftXY (round mX + round (width / 2), round mY + round (height / 2))
        (ax, ay) = (tempAx + sqSize * 4, tempAy + sqSize * 4)
        (tempBx, tempBy) = topLeftXY boardLoc
        (bx, by) = (tempBx + sqSize * 4, tempBy + sqSize * 4)

        clickedWithinBoard :: Bool
        clickedWithinBoard = ax >= bx && ax <= bx + bWidth &&
                             ay >= by && ay <= by + bWidth
            where
                bWidth = sqSize * 8


        x = round (ax - bx) `div` sqSizeI
        y = round (ay - by) `div` sqSizeI

        getOrPlace Nothing brd' = newGame'
            where
                (newSel, newBoard) = replaceNth2d x y (Piece Empty NoColor) brd' []
                newGame' = if pcolor newSel == col
                then Game state (Board newBoard) col nMvs (Just newSel) (Just (validMoves newSel (x, y) (Board newBoard))) (mX, mY) bt wt
                else Game state (Board brd') col nMvs Nothing availMvs (mX, mY) bt wt
        getOrPlace (Just piece) brd' = newGame'
            where
                (_, newBoard) = replaceNth2d x y piece brd' []

                avMoves = fromJust availMvs
                newGame' | (x, y) == start (head avMoves) =
                            Game state (Board newBoard) col nMvs Nothing Nothing (mX, mY) bt wt -- Place back
                         | (x, y) `elem` map end avMoves =
                            Game state (Board newBoard) (nextColor col) (nMvs+1) Nothing Nothing (mX, mY) bt wt -- Move piece
                         | otherwise = Game state (Board brd) col nMvs prevSel availMvs (mX, mY) bt wt -- Illegal move

        newGame = getOrPlace prevSel brd


handleInputs (EventMotion (mX, mY)) game = newGame
    where
        (Game state (Board brd) col nMvs prevSel availMvs _ bt wt) = game
        newGame = Game state (Board brd) col nMvs prevSel availMvs (mX, mY) bt wt
handleInputs _ game = game
