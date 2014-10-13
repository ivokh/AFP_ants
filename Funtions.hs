module Functions where

import CodeGenerator

import Control.Monad
import Control.Monad.State


type Dir = Int --0 (east) to 5 (north-east), clockwise

main = 
    do
        let code = unlines . map show . runCode $ combine
                searchFoodDef
                takeAndGoHomeDef
                followHomeDef
                (searchFood 0 2) --TODO: different ants should go in different directions
        writeFile "pathFollowing2.txt" code

-- | Defines searchFood for all legitimate parameters
searchFoodDef :: Code
searchFoodDef = combineList [searchFood facing dest | facing <- [0, 2, 4], dest <- [0, 2, 4]]

--TODO: jump to a different state if moving fails
------- sense even markers, and follow them (and stop leaving odd markers)
------- change direction a bit every now and them
-- | Searches for food given the dir it's facing (this should be even!) and the (even) dir it should go, and leaves a trail that can be followed home
searchFood :: Dir -> Dir -> Code
searchFood facing dest = define "searchFood" [mkParam facing, mkParam dest] $ combine 
    (turnN' (shortestTurn (dest - facing)))
    (sense Here (call "takeAndGoHome" [mkParam facing]) next Food)
    (mark ((dest + 3) `mod` 6) next)
    (move (call "searchFood" [mkParam facing, mkParam dest]) this) 
        where
            turnN' n | n > 0     = combineList $ replicate n    (turn R next)
                     | n < 0     = combineList $ replicate (-n) (turn L next)
                     | otherwise = return []
            

shortestTurn :: Int -> Int
shortestTurn n | n > 3     = 6 - n
               | n < -3    = 6 + n
               | otherwise = n

takeAndGoHomeDef :: Code
takeAndGoHomeDef = combineList [takeAndGoHome facing | facing <- [0, 2, 4]]
              
-- | Ant must be facing in an even direction              
takeAndGoHome :: Dir -> Code
takeAndGoHome facing = define "takeAndGoHome" [mkParam facing] $ combine
    (pickUp next next) --TODO search food nearby if this food particle is gone
    (combineList (replicate 3 (turn L next)))
    (move (call "followHome" [mkParam ((facing + 3) `mod` 6)]) this)
    
followHomeDef :: Code
followHomeDef = combineList ([followHome facing | facing <- [1, 3, 5]] ++ [turnNDef (call "followHome" [mkParam facing]) | facing <- [1, 3, 5]])

-- | Follows a path of odd markers, leaving a trail of odd markers, given the direction it's facing (should be odd)
followHome :: Dir -> Code
followHome facing = define "followHome" [mkParam facing] $ combine
    (follow 1)
    (follow 3)
    (follow 5)
        --TODO: what if there is no marker for some reason?
        where follow n = sense Here (call "turnN" [mkParam $ shortestTurn (n - facing), mkParam $ call "followHome" [mkParam n]]) next (Marker n)
    

-- | Defines turnN for all legitimate parameters, given the next state
turnNDef :: AntState -> Code
turnNDef st = combineList [turnN n st | n <- [-3..3]]

-- | Turns n clockWise (or -n counter-clockwise if n is negative) and then move to a given state
turnN :: Int -> AntState -> Code
turnN n st | n > 0     = define "turnN" [mkParam n, mkParam st] $ turn R (call "turnN" [mkParam (n - 1), mkParam st])
           | n < 0     = define "turnN" [mkParam n, mkParam st] $ turn L (call "turnN" [mkParam (n + 1), mkParam st])
           | otherwise = addLabel "turnN" [mkParam 0, mkParam st] st
           
{-
#############################################
Some (possibly useless) funtions for testing:
#############################################
-}
main' = 
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