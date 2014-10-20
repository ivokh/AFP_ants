module Functions where

import CodeGenerator

import Control.Monad
import Control.Monad.State


type Dir = Int --0 (east) to 5 (north-east), clockwise

foodPath :: Condition
foodPath = Marker 0

main = 
    do
        let code = unlines . map show . runCode $ combine
                --Start with assigning roles
                (assignRandom [call "searchFood" [mkParam 0, mkParam dir] | dir <- [0, 2, 4]])
                --All other code comes here (note that all states have the same properties, except for state 1 where execution starts)
                takeAndGoHomeDef
                followHomeDef
                startFollowFoodDef
                followToFoodDef
                searchFoodDef
        writeFile "pathFollowing2.ant" code
        
-- | Assigns random roles to ants, the list must be length 2 or larger, every antstate in the list has the same chance of being selected
assignRandom :: [AntState] -> Code
assignRandom xs = assignRandom' (length xs) xs where
    assignRandom' 2 (x:y:[]) = toss 2 x y
    assignRandom' n (x:xs)   = combine (toss n x next) (assignRandom' (n - 1) xs)

-- | Defines searchFood for all legitimate parameters
searchFoodDef :: Code
searchFoodDef = combineList [searchFood facing dest | facing <- [0, 2, 4], dest <- [0, 2, 4]]

--TODO: jump to a different state if moving fails
------- change direction a bit every now and them
-- | Searches for food given the dir it's facing (this should be even!) and the (even) dir it should go, and leaves a trail that can be followed home
searchFood :: Dir -> Dir -> Code
searchFood facing dest = define "searchFood" [mkParam facing, mkParam dest] $ combine 
    --Turn to the assigned direction
    (turnN' (shortestTurn (dest - facing)))
    --Sense food
    (sense Here (call "takeAndGoHome" [mkParam dest]) next Food)
    --Sense a path leading to food
    (sense Here (call "startFollowFood" [mkParam dest]) next foodPath)
    --If food or path wasn't found, move and leave a mark that leads to home
    (move next (relative 5))
    --Only leave a marker if there is no path marker already
    sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 1)
    sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 3)
    sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 5)
    (mark ((dest + 3) `mod` 6) (call "searchFood" [mkParam dest, mkParam dest]))
    --If movement fails, pick a random new even direction
    (toss 2 (call "searchFood" [mkParam dest, mkParam ((dest - 2) `mod` 6)]) (call "searchFood" [mkParam dest, mkParam ((dest + 2) `mod` 6)]))
        where
            turnN' n | n > 0     = combineList $ replicate n    (turn R next)
                     | n < 0     = combineList $ replicate (-n) (turn L next)
                     | otherwise = return []
            
-- | Gives the shortest turn for an int, where possitive means clockwise and negative means counter-clockwise
shortestTurn :: Int -> Int
shortestTurn n | m > 3     = m - 6
               | m < -3    = 6 + m
               | otherwise = m
    where m = n `mod` 6

takeAndGoHomeDef :: Code
takeAndGoHomeDef = combineList [takeAndGoHome facing | facing <- [0, 2, 4]]
              
-- | Ant must be facing in an even direction              
takeAndGoHome :: Dir -> Code
takeAndGoHome facing = define "takeAndGoHome" [mkParam facing] $ combine
    (pickUp next next) --TODO search food nearby if this food particle is gone
    (combineList (replicate 2 (turn L next)))
    (turn L (call "followHome" [mkParam ((facing + 3) `mod` 6)]))
    
followHomeDef :: Code
followHomeDef = combineList ([followHome facing | facing <- [1, 3, 5]] ++ [turnNDef (call "followHome" [mkParam facing]) | facing <- [1, 3, 5]])

-- | Follows a path of odd markers, leaving a trail of odd markers, given the direction it's facing (should be odd)
followHome :: Dir -> Code
followHome facing = define "followHome" [mkParam facing] $ combine
    (move next this) --Todo: handle collisions with other ants
    (sense Here next (relative 6) Home)
    (dropFood next)
    (combineList (replicate 3 (turn L next)))
    (call "searchFood" [mkParam ((facing + 3) `mod` 6), mkParam ((facing + 3) `mod` 6)])
    (follow 1)
    (follow 3)
    (follow 5)
    (turn R this)
        where follow n = sense Here (call "turnN" [mkParam $ shortestTurn (n - facing), mkParam $ call "followHome" [mkParam n]]) next (Marker n)
    
startFollowFoodDef :: Code
startFollowFoodDef = combineList [startFollowFood facing | facing <- [0..5]]
    
-- | Starts following a path to food
startFollowFood :: Dir -> Code
startFollowFood facing = define "startFollowFood" [mkParam facing] $
    combineList [follow n | n <- [1, 3, 5]]
        where follow n = sense Here (call "turnN" [mkParam $ shortestTurn (n + 3 - facing), mkParam $ call "followToFood" [mkParam ((n + 3) `mod` 6)]]) next (Marker n)
    
-- | Defines followToFood
followToFoodDef :: Code
followToFoodDef = combineList ([followToFood facing | facing <- [0..5]] ++
                     [turnNDef (call "followToFood" [mkParam facing]) | facing <- [0..5]] ++
                     [detectPath facing | facing <- [0..5]])
    
-- | Follows a trail that leads to food, note that paths lead to home, so we need to follow it in the opposite direction
--Idea: if food is gone, leave a marker for other ants and start clearing the trail of marker 0, other ants go home and search
followToFood :: Dir -> Code
followToFood facing = define "followToFood" [mkParam facing] $ 
    combineList [follow n | n <- [0, 2, 4]]
        where follow n = sense Here (call "turnN" [mkParam $ shortestTurn (n + 3 - facing), mkParam $ call "followToFood" [mkParam ((n + 3) `mod` 6)]]) next (Marker n)

detectPath :: Dir -> Code
detectPath facing = define "detectPath" [mkParam facing] $ combine
    (combineList [search x | x <- [Ahead, LeftAhead, RightAhead]])
    (define "move" [mkParam facing] $ move (call "followToFood" [mkParam facing]) this)
        where search x = sense x (call "move" [mkParam facing]) next foodPath
        
-- | Defines turnN for all legitimate parameters, given the next state
turnNDef :: AntState -> Code
turnNDef st = combineList [turnN n st | n <- [-3..3]]

-- | Turns n clockWise (or -n counter-clockwise if n is negative) and then move to a given state
turnN :: Int -> AntState -> Code
turnN n st | n > 0     = define "turnN" [mkParam n, mkParam st] $ turn R (call "turnN" [mkParam (n - 1), mkParam st])
           | n < 0     = define "turnN" [mkParam n, mkParam st] $ turn L (call "turnN" [mkParam (n + 1), mkParam st])
           | otherwise = addLabel "turnN" [mkParam 0, mkParam st] st
        
        
