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
        let code = unlines $ map show instructions
        writeFile "testCode.txt" code

instructions :: [Instruction]
instructions = let (instr, (_, env)) = runState (test env) (1, []) in instr

test :: Env -> Code
test env = annotate "test" $ combine
                             (toss env 2 (Jump "init1") (Jump "init2")) --Split the ants in 2 groups of approximately the same size
                             (annotate "init1" (init1 env))
                             (annotate "init2" (init2 env))
    
--initialize group 1
init1 :: Env -> Code
init1 env = combine
    (fmap concat . replicateM 10 $ dropFood env next) --just stall 10 turns trying to drop things..
    (turn env L (Jump "test")) -- ..turn left and them toss a coin again :D
    
--initialize group 2
init2 :: Env -> Code
init2 env = fmap concat . replicateM 100 $ move env next next --MOVE MOVE MOVE!!