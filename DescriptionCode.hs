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
        (toss 6 next (relative 2))
        (sense Ahead next (jump "track1") Friend)
        (turn L next)
        (toss 5 next (relative 2))
        (sense Ahead next (jump "track1") Friend)
        (turn L next)
        (toss 4 next (relative 2))
        (sense Ahead next (jump "track1") Friend)
        (turn L next)
        (toss 3 next (relative 2))
        (sense Ahead next (jump "track1") Friend)
        (turn L next)
        (toss 2 next (relative 2))
        (sense Ahead next (jump "track1") Friend)
        (turn L (relative (-14)))
        (testEnemyHome "initBlockEnemyHome" "searchFood")
        initBlockEnemyHome
        searchFood
        track1

searchFood :: Code
searchFood = annotate "searchFood" $ combine
        (sense Ahead next (call "randomMoveL" [(mkParam "searchFood'")]) Food)
        (move (jump "pickup") (jump "searchFood"))
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
        (sense Ahead next (call "randomMoveL" [(mkParam "searchHome'")]) Home)
        (move (jump "drop") (jump "searchHome")) 
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

track1 :: Code
track1 = annotate "track1" $
        moveAndTest 70
        
moveAndTest :: Int -> Code
moveAndTest 1  = moveAndTestTerm 70
moveAndTest 38 = moveAndTest' 38 nextMove nextReturn
        where
            nextMove succes error   = (turn L succes)
            nextReturn succes error = (turn R succes)
moveAndTest 58 = moveAndTest' 58 nextMove nextReturn
        where
            nextMove succes error   = (turn L succes)
            nextReturn succes error = (turn R succes)
moveAndTest n  = moveAndTest' n nextMove nextReturn
        where
            nextMove succes error   = (move succes error)
            nextReturn succes error = (move succes error)

moveAndTest' n nextMove nextReturn = define "moveAndTest" [mkParam (n::Int)] $ combine
        (sense Here next (call "moveNext" [mkParam (n::Int)]) Food)
        (sense Here (call "moveNext" [mkParam (n::Int)]) next Home)
        -- found food
        (pickUp 
            (call "moveBack" [mkParam "returnTest", mkParam (n::Int)])
            (call "moveBack" [mkParam "returnTest", mkParam (n::Int)])
        )
        (moveBack "returnTest" n)
        (moveBack "moveAndTest" n)
        -- end found food
        (define "moveNext" [mkParam (n::Int)] 
            (let
                succes = (call "moveAndTest" [mkParam ((n::Int) - 1)]) 
                error  = (call "moveBack" [mkParam "returnTest", mkParam (n::Int)])
            in nextMove succes error)
        )
        -- return path
        (define "returnTest" [mkParam ((n::Int) - 1)] 
            (sense Here next (call "returnPath" [mkParam ((n::Int) - 1)]) Home)
        )
        -- found home
        (dropFood
            (call "moveBack" [mkParam "moveAndTest", mkParam ((n::Int) - 1)])
        )
        (define "returnPath" [mkParam ((n::Int) - 1)] 
            (let
                succes = (call "returnTest" [mkParam (n::Int)]) 
                error  = (call "moveBack" [mkParam "moveAndTest", mkParam ((n::Int) - 1)])
            in nextReturn succes error)
        )
        -- return path with food
        (define "returnTestWithFood" [mkParam ((n::Int) - 1)] 
            (sense Here next (call "returnPathWithFood" [mkParam ((n::Int) - 1)]) Home)
        )
        -- found home
        (dropFood
            (call "moveBack" [mkParam "moveAndTest", mkParam ((n::Int) - 1)])
        )
        (define "returnPathWithFood" [mkParam ((n::Int) - 1)] 
            (mark 1 next)
        )
        (let
            succes = (call "returnTestWithFood" [mkParam (n::Int)])
            error  = (call "moveBack" [mkParam "moveAndTest", mkParam ((n::Int) - 1)])
        in nextReturn succes error)
        (moveAndTest (n - 1))

moveAndTestTerm n = define "moveAndTest" [mkParam (1::Int)] $ combine
        (sense Ahead
            (call "moveBack" [mkParam "returnTest", mkParam (1::Int)])
            (call "moveBack" [mkParam "returnTest", mkParam (1::Int)])
        Food)
        -- return path
        (define "returnTest" [mkParam (1::Int)] 
            (move
                (call "returnTest" [mkParam (2::Int)]) 
                (call "moveBack" [mkParam "moveAndTest", mkParam (2::Int)])
            )
        )
        (define "returnTest" [mkParam (n::Int)] 
            (sense Here 
                next
                (call "moveBack" [mkParam "moveAndTest", mkParam (n::Int)])
            Home)
        )
        (dropFood
            (call "moveBack" [mkParam "moveAndTest", mkParam (n::Int)])
        )
        (define "returnTestWithFood" [mkParam (n::Int)] 
            (sense Here 
                next
                (call "moveBack" [mkParam "moveAndTest", mkParam (n::Int)])
            Home)
        )
        (define "moveAndTest" [mkParam ((n::Int) + 1)]
            (sense Here 
                (call "moveBack" [mkParam "returnTest", mkParam (n::Int)])
                (call "moveBack" [mkParam "returnTest", mkParam (n::Int)])
            Home)
        )
        (moveBack "returnTest" 1)
        (moveBack "moveAndTest" 1)
        (moveBack "moveAndTest" n)
        (moveBack "moveAndTest" (n + 1))

moveBack :: String -> Int -> Code
moveBack r a = define "moveBack" [mkParam r, mkParam (a::Int)] $ combine
        (turn R next)
        (turn R next)
        (turn R (call r [mkParam (a::Int)]))
        
--tries to move n times
move' :: Int -> Code
move' n = if n == 1 then move next next else combine (move next next) (define "move" [mkParam (n - 1)] $ move' (n - 1))
