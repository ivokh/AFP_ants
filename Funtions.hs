module Functions where

import CodeGenerator

import Control.Monad
import Control.Monad.State


{-
#############################################
Some (possibly useless) funtions for testing:
#############################################
-}

main = 
    do
        let code = unlines $ map show (runCode test)
        writeFile "testCode2.txt" code

test :: Code
test = annotate "test" $ combine
        (toss 2 (jump "init1") (call "move" [mkParam (100 :: Int)])) --Split the ants in 2 groups of approximately the same size
        (annotate "init1" init1)
        (define "move" [mkParam (100 :: Int)] (move' 100))
    
--initialize group 1
init1 :: Code
init1 = combine
    (fmap concat . replicateM 10 $ dropFood next) --just stall 10 turns trying to drop things..
    (turn L (call "test" [])) -- ..turn left and them toss a coin again :D
    
--initialize group 2
init2 :: Code
init2 = move' 100 --MOVE MOVE MOVE!!

--tries to move n times
move' :: Int -> Code
move' n = if n == 1 then move next next else combine (move next next) (define "move" [mkParam (n - 1)] $ move' (n - 1))