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
        writeFile "testCode.txt" code

test :: Code
test = annotate "test" $ combine
        (toss 2 (Jump "init1") (Jump "init2")) --Split the ants in 2 groups of approximately the same size
        (annotate "init1" init1)
        (annotate "init2" init2)
    
--initialize group 1
init1 :: Code
init1 = combine
    (fmap concat . replicateM 10 $ dropFood next) --just stall 10 turns trying to drop things..
    (turn L (Jump "test")) -- ..turn left and them toss a coin again :D
    
--initialize group 2
init2 :: Code
init2 = fmap concat . replicateM 100 $ move next next --MOVE MOVE MOVE!!