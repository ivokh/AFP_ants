module DescriptionCode where

import CodeGenerator

import Control.Monad
import Control.Monad.State

{-
#############################################
Functions for generating the code given in the task description
under "A Brief Look at the State Machine"
#############################################
-}

main = 
    do
        let code = unlines $ map show (runCode searchFood')
        writeFile "testAnt.ant" code

searchFood' :: Code
searchFood' = annotate "searchFood'" $ combine
        (testEnemyHome "initBlockEnemyHome" "searchFood")
        initBlockEnemyHome
        searchFood

searchFood :: Code
searchFood = annotate "searchFood" $ combine
        (sense Here next (call "randomMoveL" [(mkParam "searchFood'")]) Food)
        pickup
        (randomMoveL "searchFood'")
        searchHome'

pickup :: Code
pickup = annotate "pickup" $ combine
        (sense Here (call "randomMoveL" [(mkParam "searchFood'")]) next Home)
        (pickUp (jump "searchHome") (jump "searchFood"))

searchHome' :: Code
searchHome' = annotate "searchHome'" $ combine
        (testEnemyHome "initBlockEnemyHome" "searchHome")
--        initBlockEnemyHome
        searchHome
        
searchHome :: Code
searchHome = annotate "searchHome" $ combine
        (sense Here next (call "randomMoveL" [(mkParam "searchHome'")]) Home)
        --(move (jump "drop") (jump "searchHome'")) 
        doDrop
        (randomMoveL "searchHome'")

doDrop :: Code
doDrop = annotate "drop"
        (dropFood (jump "searchFood"))
        
randomMoveL :: String -> Code
randomMoveL n = define "randomMoveL" [(mkParam n)] $ combine
        (toss 3 next (call "randomMoveR" [(mkParam n)]))
        (turn L (jump n))
        (randomMoveR n)

randomMoveR :: String -> Code
randomMoveR n = define "randomMoveR" [(mkParam n)] $ combine
        (toss 2 next (call "moveForward" [(mkParam n)]))
        (turn R (jump n))
        (moveForward n)

moveForward :: String -> Code
moveForward n = define "moveForward" [(mkParam n)] $ 
        (move (jump n) (call "randomMoveL" [(mkParam n)]))
    
testEnemyHome :: String -> String -> Code
testEnemyHome t f = define "testEnemyHome" [(mkParam t), (mkParam f)] $
        (sense Here (jump t) (jump f) FoeHome)

initBlockEnemyHome :: Code
initBlockEnemyHome = annotate "initBlockEnemyHome" $ combine
        (turn R next)
        (sense Ahead (jump "initBlockEnemyHome") (jump "blockEnemyHome") FoeHome)
        blockEnemyHome

blockEnemyHome :: Code
blockEnemyHome = annotate "blockEnemyHome" $ combine
        (sense Ahead next (relative 2) FoeHome)
        (move (jump "blockEnemyHome") (jump "blockEnemyHome"))
        (turn L (jump "blockEnemyHome"))
