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
                followToFoodDef
                searchFoodDef
                (guardEntrance RightAhead)
                (guardEntrance LeftAhead)
                (guardEntrance' Ahead)
                guardEntrance''
                repositionFood
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
    (sense Here (call "followToFood" [mkParam dest]) next foodPathCond)
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
    (unMark foodGone next)
    (combineList (replicate 2 (turn L next)))
    --Start following the path home, leaving a foodpath trail
    (turn L (call "searchHome" [mkParam ((facing + 3) `mod` 6), mkParam MarkPath]))
    
searchHomeDef :: Code
searchHomeDef = combineList ([searchHome facing leaveMark | facing <- [0..5], leaveMark <- [MarkPath, UnmarkPath, DoNothing]] ++ 
                [turnNDef (call "searchHome" [mkParam facing, mkParam leaveMark]) | facing <- [0..5], leaveMark <- [MarkPath, UnmarkPath, DoNothing]])

data PathInstr = MarkPath | UnmarkPath | DoNothing
    deriving (Show, Eq)
                
-- | Follows a path of odd markers, possibly leaving a trail of foodpath markers, given the direction it's facing (should be odd)
searchHome :: Dir -> PathInstr -> Code
searchHome facing leaveMark = define "searchHome" [mkParam facing, mkParam leaveMark] $ combine
    --Mark or unmark if necessary
    (case leaveMark of
        MarkPath   -> combine (mark foodPath next) (unMark foodGone next)
        UnmarkPath -> combine (unMark foodPath next) (mark foodGone next)
        _          -> (return []) :: Code)
    --When cleaning a path, let other ants know so they move out of the way, and keep trying to move. Otherwise other ants have priority
    (tryMove facing)
    --If moving was succesful, sense if at home
    (sense Here next (relative 5) Home)
    (dropFood next)
    (combineList (replicate 2 (turn L next)))
    (turn L (call "searchFood" [mkParam ((facing + 3) `mod` 6), mkParam ((facing + 3) `mod` 6)]))
    --If not at home, sense a path
    (follow 1 next)
    (follow 3 next)
    --If no path is found, keep looking
    (follow 5 (call "searchHome" [mkParam facing, mkParam DoNothing]))
        where --If the path can be followed, keep leaving marks
              follow n st = sense Here (call "turnN" [mkParam $ shortestTurn (n - facing), mkParam $ call "searchHome" [mkParam n, mkParam leaveMark]]) st (Marker n)
              --If moving fails, pick a random direction and try to find another path there, otherwise go to next
              tryMove :: Dir -> Code
              tryMove facing = combine
                  (move (relative 10) next)
                  (toss 2 next (relative 2))
                  (toss 2 (relative 2) (relative 3))
                  (toss 2 (relative 3) (relative 5))
                  (turn L (call "searchHome" [mkParam ((facing - 1) `mod` 6), mkParam DoNothing]))
                  (turn R (call "searchHome" [mkParam ((facing + 1) `mod` 6), mkParam DoNothing]))
                  (turn L next)
                  (turn L (call "searchHome" [mkParam ((facing - 2) `mod` 6), mkParam DoNothing]))
                  (turn R next)
                  (turn R (call "searchHome" [mkParam ((facing + 2) `mod` 6), mkParam DoNothing]))
    
-- | Defines followToFood
followToFoodDef :: Code
followToFoodDef = combineList ([followToFood facing | facing <- [0..5]] ++
                     [turnNDef (call "detectPath" [mkParam facing]) | facing <- [0..5]] ++
                     [detectPath facing | facing <- [0..5]])
    
-- | Follows a trail that leads to food, note that paths lead to home, so we need to follow it in the opposite direction
followToFood :: Dir -> Code
followToFood facing = define "followToFood" [mkParam facing] $ combine
    --Turn to the direction that most likely has the next link in the path and look for the next link
    (combineList [follow n | n <- [1, 3, 5]])
    --This should never happen:
    (turn L this)
        where follow n = sense Here (call "turnN" [mkParam $ shortestTurn (n + 3 - facing), mkParam $ call "detectPath" [mkParam ((n + 3) `mod` 6)]]) next (Marker n)

-- | Detect the next link a path leading to food and follow it
detectPath :: Dir -> Code
detectPath facing = define "detectPath" [mkParam facing] $ combine
    --Check if you're at the food
    (sense Here next (relative 2) Food)
    --Only pick up food if not at home
    (sense Here next (call "takeAndGoHome" [mkParam facing]) Home)
    --Look ahead
    (combineList [search x next facing | x <- [Ahead, LeftAhead, RightAhead]])
    --If there's no path ahead it must mean a sharp turn was made, or the food is gone
    (combineList $ replicate 3 (turn L next))
    (search LeftAhead next ((facing + 3) `mod` 6))
    (search RightAhead next ((facing + 3) `mod` 6))
    --Start cleaning up the trail if it's realy gone
    (unMark foodPath next)
    (mark foodGone (call "searchHome" [mkParam ((facing + 3) `mod` 6), mkParam UnmarkPath]))
    
    --TODO: Search the neighborhood for food
    --TODO: (zowel foodpath als homepath moeten uitgebreid worden tijdens het in de buurt zoeken)
    
    --Helper function
    (define "move" [mkParam facing] $ move (call "followToFood" [mkParam facing]) (call "detectPath" [mkParam facing]))
        where search :: SenseDir -> AntState -> Dir -> Code
              search x st facing = combine (sense x (call "searchFood" [mkParam facing, mkParam facing]) next foodGoneCond) (sense x (call "move" [mkParam facing]) st foodPathCond)
        
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
    (mark 4 next)
    (move (jump "start") (jump "start"))
    (annotate "step1" $ combine
        (combineList $ replicate 4 (pickUp next next))  -- Wait untill the first marker is dropped
        (sense Ahead next (jump "step2") (Marker 4))  
        (mark 4 next)
        (combineList $ replicate 4 (pickUp next next))
        (sense LeftAhead (jump "guardEntrance''") this Friend)
    )
    (annotate "step2" $ combine
        (sense Ahead (jump "step4") next Home)
        (sense RightAhead next (jump "step3") (Marker 4))
        (move (call "guardEntrance" [mkParam RightAhead]) next)
    )
    (annotate "step3" $ combine
        (sense LeftAhead next (jump "start") (Marker 4))
        (move (call "guardEntrance" [mkParam LeftAhead]) next)     
    )
    (annotate "step4" $ combine
        (combineList $ replicate 3 (pickUp next next)) -- Wait to make sure the second marker has been droppd
        (sense RightAhead next (jump "step5") (Marker 4))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )
    (annotate "step5" $ combine
        (sense LeftAhead next (jump "start") (Marker 4))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )

-- For the two ants at the front of the defending formation
guardEntrance :: SenseDir -> Code
guardEntrance s = define "guardEntrance" [mkParam s] $ combine
    (sense s next this Foe)
    (move next this)
    (combineList $ replicate 3 (turn L next))
    (move next this)
    (turn L next)
    (turn L next)
    (turn L (call "guardEntrance" [mkParam s]))

-- For the two ants behind the two frontal ants
guardEntrance' :: SenseDir -> Code
guardEntrance' s = define "guardEntrance'" [mkParam s] $ combine
    (sense s this next Friend)
    (move next this)
    (combineList $ replicate 3 (turn L next))
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
        (combineList $ replicate 3 (turn L next))
        (move next this)
        (dropFood next)
        (combineList $ replicate 3 (turn L next))
        (sense Ahead (jump "collectKill") (jump "returnToPosition") Food)  
    )
    (annotate "returnToPosition" $ combine
        (combineList $ replicate 3 (turn L next))
        (move next this)
        (turn L next)
        (turn L next)
        (turn L (jump "guardEntrance''"))
    )
    (sense Ahead (jump "collectKill") (jump "returnToPosition") Food)

repositionFood :: Code
repositionFood = annotate "repositionFood" $ combine
    (turn L this)



