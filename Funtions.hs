module Functions where

import CodeGenerator

import Control.Monad
import Control.Monad.State


type Dir = Int --0 (east) to 5 (north-east), clockwise

--marker semantics
foodPath, foodGone :: Marker
foodPath = 2
foodGone = 4
foodPathCond, foodGoneCond :: Condition
foodPathCond = Marker foodPath
foodGoneCond = Marker foodGone

main = 
    do
        let code = unlines . map show . runCode $ combine
                defendHome
                --Start with assigning roles
                (annotate "start" (assignRandom [call "searchFood" [mkParam 0, mkParam dir] | dir <- [0, 2, 4]]))
                --All other code comes here (note that all states have the same properties, except for state 1 where execution starts)
                takeAndGoHomeDef
                searchHomeDef
                startFollowFoodDef
                followToFoodDef
                searchFoodDef
                (guardEntrance RightAhead)
                (guardEntrance LeftAhead)
                (guardEntrance' LeftAhead)
                (guardEntrance' RightAhead)
                (guardEntrance' Ahead)
                guardEntrance''
        writeFile "pathFollowing2.ant" code
        
-- | Assigns random roles to ants, the list must be length 2 or larger, every antstate in the list has the same chance of being selected
assignRandom :: [AntState] -> Code
assignRandom xs = assignRandom' (length xs) xs where
    assignRandom' 2 (x:y:[]) = toss 2 x y
    assignRandom' n (x:xs)   = combine (toss n x next) (assignRandom' (n - 1) xs)

-- | Defines searchFood for all legitimate parameters
searchFoodDef :: Code
searchFoodDef = combineList [searchFood facing dest | facing <- [0..5], dest <- [0..5]]

--TODO: change direction a bit every now and them
-- | Searches for food given the dir it's facing (this should be even!) and the (even) dir it should go, and leaves a trail that can be followed home
searchFood :: Dir -> Dir -> Code
searchFood facing dest' = define "searchFood" [mkParam facing, mkParam dest'] $ combine 
    --Turn to the assigned direction
    (turnN' (shortestTurn (dest - facing)))
    --Sense food
    (sense Here next (relative 2) Food)
    --Only pick up food if not at home
    (sense Here next (call "takeAndGoHome" [mkParam dest]) Home)
    --Sense a path leading to food
    (sense Here (call "startFollowFood" [mkParam dest]) next foodPathCond)
    --If food or path wasn't found, move and leave a mark that leads to home
    (move next (relative 5))
    --Only leave a marker if there is no path marker already
    (sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 1))
    (sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 3))
    (sense Here (call "searchFood" [mkParam dest, mkParam dest]) next (Marker 5))
    (mark ((dest + 3) `mod` 6) (call "searchFood" [mkParam dest, mkParam dest]))
    --If movement fails, pick a random new even direction
    (toss 2 (call "searchFood" [mkParam dest, mkParam ((dest - 2) `mod` 6)]) (call "searchFood" [mkParam dest, mkParam ((dest + 2) `mod` 6)]))
        where
            turnN' n | n > 0     = combineList $ replicate n    (turn R next)
                     | n < 0     = combineList $ replicate (-n) (turn L next)
                     | otherwise = return []
            --The destination should always be even
            dest = dest' - (dest' `mod` 2)
            
-- | Gives the shortest turn for an int, where possitive means clockwise and negative means counter-clockwise
shortestTurn :: Int -> Int
shortestTurn n | m > 3     = m - 6
               | m < -3    = 6 + m
               | otherwise = m
    where m = n `mod` 6

takeAndGoHomeDef :: Code
takeAndGoHomeDef = combineList [takeAndGoHome facing | facing <- [0..5]]
              
-- | Pick up food and go home    
takeAndGoHome :: Dir -> Code
takeAndGoHome facing = define "takeAndGoHome" [mkParam facing] $ combine
    (pickUp next next)
    (mark foodPath next)
    (combineList (replicate 2 (turn L next)))
    --Start following the path home, leaving a foodpath trail
    (turn L (call "searchHome" [mkParam ((facing + 3) `mod` 6), mkParam True]))
    
searchHomeDef :: Code
searchHomeDef = combineList ([searchHome facing leaveMark | facing <- [0..5], leaveMark <- [True, False]] ++ 
                [turnNDef (call "searchHome" [mkParam facing, mkParam leaveMark]) | facing <- [0..5], leaveMark <- [True, False]])

-- | Follows a path of odd markers, possibly leaving a trail of foodpath markers, given the direction it's facing (should be odd)
searchHome :: Dir -> Bool -> Code
searchHome facing leaveMark = define "searchHome" [mkParam facing, mkParam leaveMark] $ combine
    (tryMove facing)
    --If moving was succesful, leave a marker and sense if at home
    (if leaveMark then (mark foodPath next) else return [])
    (sense Here next (relative 5) Home)
    (dropFood next)
    (combineList (replicate 2 (turn L next)))
    (turn L (call "searchFood" [mkParam ((facing + 3) `mod` 6), mkParam ((facing + 3) `mod` 6)]))
    --If not at home, sense a path
    (follow 1 next)
    (follow 3 next)
    --If no path is found, keep looking
    (follow 5 (call "searchHome" [mkParam facing, mkParam False]))
        where --If the path can be followed, keep leaving marks
              follow n st = sense Here (call "turnN" [mkParam $ shortestTurn (n - facing), mkParam $ call "searchHome" [mkParam n, mkParam leaveMark]]) st (Marker n)
              --If moving fails, pick a random direction and try to find another path there, otherwise go to next
              tryMove facing = combine
                  (move (relative 10) next)
                  (toss 2 next (relative 2))
                  (toss 2 (relative 2) (relative 3))
                  (toss 2 (relative 3) (relative 5))
                  (turn L (call "searchHome" [mkParam ((facing - 1) `mod` 6), mkParam False]))
                  (turn R (call "searchHome" [mkParam ((facing + 1) `mod` 6), mkParam False]))
                  (turn L next)
                  (turn L (call "searchHome" [mkParam ((facing - 2) `mod` 6), mkParam False]))
                  (turn R next)
                  (turn R (call "searchHome" [mkParam ((facing + 2) `mod` 6), mkParam False]))
    
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

-- | Detect the next link a path leading to food and follow it
detectPath :: Dir -> Code
detectPath facing = define "detectPath" [mkParam facing] $ combine
    --Check if you're at the food
    (sense Here next (relative 2) Food)
    --Only pick up food if not at home
    (sense Here next (call "takeAndGoHome" [mkParam facing]) Home)
    --Look ahead
    (combineList [search x | x <- [Ahead, LeftAhead, RightAhead]])
    --If there's no path ahead it must mean a sharp turn was made
    (combineList $ replicate 3 (turn L next))
    (search LeftAhead)
    (search RightAhead)
    --No sharp turn either, so we must be at the end of the trail, BUT there's no food!
    --Search the neighborhood for food, and start cleaning up the trail if it's realy gone
    --
    --TODO (zowel foodpath als homepath moeten uitgebreid worden tijdens het in de buurt zoeken)
    --
    (define "move" [mkParam facing] $ move (call "followToFood" [mkParam facing]) this)
        where search x = sense x (call "move" [mkParam facing]) next foodPathCond
        
-- | Defines turnN for all legitimate parameters, given the next state
turnNDef :: AntState -> Code
turnNDef st = combineList [turnN n st | n <- [-3..3]]

-- | Turns n clockWise (or -n counter-clockwise if n is negative) and then move to a given state
turnN :: Int -> AntState -> Code
turnN n st | n > 0     = define "turnN" [mkParam n, mkParam st] $ turn R (call "turnN" [mkParam (n - 1), mkParam st])
           | n < 0     = define "turnN" [mkParam n, mkParam st] $ turn L (call "turnN" [mkParam (n + 1), mkParam st])
           | otherwise = addLabel "turnN" [mkParam 0, mkParam st] st
        
        
defendHome :: Code
defendHome = annotate "defendHome" $ combine
    (sense Ahead (jump "step1") next Home)
    (sense LeftAhead (jump "step1") next Home)
    (sense RightAhead (jump "step1") next Home)
    (mark 0 next)
    (move (jump "start") (jump "start"))
    (annotate "step1" $ combine
        (pickUp next next)
        (pickUp next next)
        (pickUp next next) 
        (pickUp next next) -- Wait untill the first marker is dropped
        (sense Ahead next (jump "step2") (Marker 0))  
        (mark 0 next)
        (pickUp next next)
        (pickUp next next)
        (pickUp next next)
        (pickUp next next)
        (sense LeftAhead (jump "guardEntrance''") this Friend)
    )
    (annotate "step2" $ combine
        (sense Ahead (jump "step4") next Home)
        (sense RightAhead next (jump "step3") (Marker 0))
        (move (call "guardEntrance" [mkParam RightAhead]) next)
    )
    (annotate "step3" $ combine
        (sense LeftAhead next (jump "start") (Marker 0))
        (move (call "guardEntrance" [mkParam LeftAhead]) next)     
    )
    (annotate "step4" $ combine
        (pickUp next next)
        (pickUp next next)
        (pickUp next next) -- Wait to make sure the second marker has been droppd
        (sense RightAhead next (jump "step5") (Marker 0))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )
    (annotate "step5" $ combine
        (sense LeftAhead next (jump "start") (Marker 0))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )

-- For the two ants at the front of the defending formation
guardEntrance :: SenseDir -> Code
guardEntrance s = define "guardEntrance" [mkParam s] $ combine
    (sense s next this Foe)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L next)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L (call "guardEntrance" [mkParam s]))

-- For the two ants behind the two frontal ants
guardEntrance' :: SenseDir -> Code
guardEntrance' s = define "guardEntrance'" [mkParam s] $ combine
    (sense s this next Friend)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L next)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L (call "guardEntrance'" [mkParam s]))

guardEntrance'' :: Code
guardEntrance'' = annotate "guardEntrance''" $ combine
    -- Move forward one step when the ants in front of this one also move
    (sense LeftAhead this next Friend)
    (move next this)
    (annotate "collectKill" $ combine
        (move next this)
        (pickUp next next)
        (turn L next)
        (turn L next)
        (turn L next)
        (move next this)
        (dropFood next)
        (turn L next)
        (turn L next)
        (turn L next)
        (sense Ahead (jump "collectKill") (jump "returnToPosition") Food)  
    )
    (annotate "returnToPosition" $ combine
        (turn L next)
        (turn L next)
        (turn L next)
        (move next this)
        (turn L next)
        (turn L next)
        (turn L (jump "guardEntrance''"))
    )
    (sense Ahead (jump "collectKill") (jump "returnToPosition") Food)

    

-- Wait for n turns
--wait :: Int -> Code
--wait n = define "wait" [mkParam n] $ combine
    


