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
                (annotate "start" (assignRandom [call "searchFood" [mkParam NoTurn, mkParam 0, mkParam dir] | dir <- [0, 2, 4]]))
                --All other code comes here (note that all states have the same properties, except for state 1 where execution starts)
                takeAndGoHomeDef
                searchHomeDef
                followToFoodDef
                searchFoodDef
                defendFood
                repositionFoodInit
                repositionFood
                findDropPlace
        writeFile "pathFollowing2.ant" code
        
-- | Assigns random roles to ants, the list must be length 2 or larger, every antstate in the list has the same chance of being selected
assignRandom :: [AntState] -> Code
assignRandom xs = assignRandom' (length xs) xs where
    assignRandom' 2 (x:y:[]) = toss 2 x y
    assignRandom' n (x:xs)   = combine (toss n x next) (assignRandom' (n - 1) xs)

-- | Defines searchFood for all legitimate parameters
searchFoodDef :: Code
searchFoodDef = combineList [searchFood turn facing dest | facing <- [0..5], dest <- [0..5], turn <- [LeftTurn, RightTurn, NoTurn]]

data LastTurn = LeftTurn | RightTurn | NoTurn
    deriving (Eq, Show)

--TODO: change direction a bit every now and them
-- | Searches for food given the dir it's facing (this should be even!) and the (even) dir it should go, and leaves a trail that can be followed home
searchFood :: LastTurn -> Dir -> Dir -> Code
searchFood lastTurn facing dest' = define "searchFood" [mkParam lastTurn, mkParam facing, mkParam dest'] $ combine 
    --Turn to the assigned direction
    (turnN' (shortestTurn (dest - facing)))
    --Sense food
    (sense Here next (relative 2) Food)
    --Only pick up food if not at home
    (sense Here next (call "takeAndGoHome" [mkParam dest]) Home)
    --Sense a path leading to food
    (sense Here (call "followToFood" [mkParam dest]) next foodPathCond)
    --If food or path wasn't found, move and leave a mark that leads to home
    (move next (call "searchFood_" [mkParam lastTurn, mkParam facing, mkParam dest]))
    --Only leave a marker if there is no path marker already
    (sense Here (relative 4) next (Marker 1))
    (sense Here (relative 3) next (Marker 3))
    (sense Here (relative 2) next (Marker 5))
    (mark ((dest + 3) `mod` 6) next)
    --Sometimes, pick a new direction
    (toss 10 next (call "searchFood" [mkParam lastTurn, mkParam dest, mkParam dest]))
    (case lastTurn of
       --Don't keep making the same turn!
       LeftTurn -> (combine (turn R next) (turn R (call "searchFood" [mkParam NoTurn, mkParam ((dest + 2) `mod` 6), mkParam ((dest + 2) `mod` 6)])) :: Code)
       RightTurn -> combine (turn L next) (turn L (call "searchFood" [mkParam NoTurn, mkParam ((dest - 2) `mod` 6), mkParam ((dest - 2) `mod` 6)]))
       NoTurn    -> combine (toss 2 next (relative 3))
                            (turn R next)
                            (turn R (call "searchFood" [mkParam RightTurn, mkParam ((dest + 2) `mod` 6), mkParam ((dest + 2) `mod` 6)]))
                            (turn L next)
                            (turn L (call "searchFood" [mkParam LeftTurn, mkParam ((dest - 2) `mod` 6), mkParam ((dest - 2) `mod` 6)])))
    --If movement fails, pick a random new even direction
    (define "searchFood_" [mkParam lastTurn, mkParam facing, mkParam dest] $ toss 2 (call "searchFood" [mkParam NoTurn, mkParam dest, mkParam ((dest - 2) `mod` 6)]) (call "searchFood" [mkParam NoTurn, mkParam dest, mkParam ((dest + 2) `mod` 6)]))
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
    --If pickup fails, go search, otherwise go home
    (pickUp (relative 3) next)
    (unMark foodPath next)
    (mark foodGone (call "searchFood" [mkParam NoTurn, mkParam facing, mkParam facing]))
    --Pickup succesful: go home
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
    (sense Ahead (jump "findDropPlace") next Home)
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

{-
 - Find the place to drop food.
 - - => home tile.
 - . => not home tile.
 - G => guard
 - M => drop place
 - n => positions 1 - 4
 -
 -    - - - .
 -   - - - + +
 -  - - - + M 4
 -   - - - + + 3
 -    - - - 1 2
 -
 -    - - - .
 -   - - - + +
 -  - - - + M .
 -   - - - + + .
 -    - - - * 3
 -   - - - 1 2
 -
 -}
findDropPlace :: Code
findDropPlace = annotate "findDropPlace" $ combine
    -- Check if not already inside home
    (sense Here next (jump "startFindDropPlace") Home)
    (annotate "getOutOfHome" (move (relative 4) next))
    (move (relative 3) next)
    (move (relative 2) next)
    (turn L (relative (-3)))
    (sense Here (relative (-4)) next Home)
    (sense Ahead (jump "startFindDropPlace") next Home)
    (turn L (relative (-1)))
    -- Turn to align with the home edge.
    (annotate "startFindDropPlace" (sense Here (jump "getOutOfHome") next Home))
    (turn R next)
    (sense Ahead (relative (-1)) next Home)
    -- Move beside the home edge until an friend in found
    (move next (relative 3))
    (annotate "moveToDropPlace" (sense LeftAhead (relative (-1)) next Home))
    (turn L (relative (-2)))
    -- Move around friend
    -- position 1
    (turn R next)
    tryMove
    -- position 2
    (turn L next)
    tryMove
    -- position 3
    (turn L next)
    (sense Ahead next (relative 2) Friend) -- not there jet
    (turn R (relative (-3)))
    tryMove
    -- position 4
    (sense Ahead (relative 3) next Friend)
    (sense Ahead next (jump "moveToDropPlace") Home)
    (turn R (relative (-1)))
    (turn L next)
    --
    (move next this)
    (dropFood next)
    (move next this)
    (combineList $ replicate 3 (turn L next))
    (sense Here (jump "guardEntrance''") (jump "guardEntrance''") Friend)
    where tryMove :: Code
          tryMove = combine
           (move (relative 3) next)
           (move (relative 2) next)
           (move next (call "searchHome" [mkParam 0, mkParam DoNothing]))
        
    
-- | Defines followToFood
followToFoodDef :: Code
followToFoodDef = combineList ([followToFood facing | facing <- [0..5]] ++
                     [turnNDef (call "detectPath" [mkParam facing]) | facing <- [0..5]] ++
                     [detectPath facing | facing <- [0..5]])
    
-- | Follows a trail that leads to food, note that paths lead to home, so we need to follow it in the opposite direction
followToFood :: Dir -> Code
followToFood facing = define "followToFood" [mkParam facing] $ combine
    --Turn to the direction that most likely has the next link in the path and look for the next link
    (follow 1 next)
    (follow 3 next)
    (follow 5 (call "detectPath" [mkParam facing]))
        where follow n st = sense Here (call "turnN" [mkParam $ shortestTurn (n + 3 - facing), mkParam $ call "detectPath" [mkParam ((n + 3) `mod` 6)]]) st (Marker n)

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
    --We don't like sharp turns, so we remove the marker
    (unMark foodPath next)
    (combineList $ replicate 3 (turn L next))
    (search LeftAhead next ((facing + 3) `mod` 6))
    (search RightAhead next ((facing + 3) `mod` 6))
    --Start cleaning up the trail if it's realy gone
    (mark foodGone (call "searchHome" [mkParam ((facing + 3) `mod` 6), mkParam UnmarkPath]))
    --Helper function
    (define "move" [mkParam facing, mkParam Ahead] $ tryMove facing (call "followToFood" [mkParam facing]))
    (define "move" [mkParam facing, mkParam LeftAhead] $ turn L (call "move" [mkParam ((facing - 1) `mod` 6), mkParam Ahead]))
    (define "move" [mkParam facing, mkParam RightAhead] $ turn R (call "move" [mkParam ((facing + 1) `mod` 6), mkParam Ahead]))
    --Definition of tryMove
    (tryMove facing (call "searchFood" [mkParam NoTurn, mkParam facing, mkParam facing]))
        where search :: SenseDir -> AntState -> Dir -> Code
              search x st facing = combine (sense x (call "searchFood" [mkParam NoTurn, mkParam facing, mkParam facing]) next foodGoneCond) (sense x (call "move" [mkParam facing, mkParam x]) st foodPathCond)
              --If moving fails, pick a random direction and search for food, otherwise go to st
              tryMove :: Dir -> AntState -> Code
              tryMove facing st = define "tryMove_" [mkParam facing, mkParam st] $ combine
                  (move st next)
                  (toss 2 next (relative 2))
                  (toss 2 (relative 2) (relative 3))
                  (toss 2 (relative 3) (relative 5))
                  (turn L (call "tryMove_" [mkParam ((facing - 1) `mod` 6), mkParam $ call "searchFood" (mkParam NoTurn:(replicate 2 $ mkParam ((facing - 1) `mod` 6)))]))
                  (turn R (call "tryMove_" [mkParam ((facing + 1) `mod` 6), mkParam $ call "searchFood" (mkParam NoTurn:(replicate 2 $ mkParam ((facing + 1) `mod` 6)))]))
                  (turn L next)
                  (turn L (call "tryMove_" [mkParam ((facing - 2) `mod` 6), mkParam $ call "searchFood" (mkParam NoTurn:(replicate 2 $ mkParam ((facing - 2) `mod` 6)))]))
                  (turn R next)
                  (turn R (call "tryMove_" [mkParam ((facing + 2) `mod` 6), mkParam $ call "searchFood" (mkParam NoTurn:(replicate 2 $ mkParam ((facing + 2) `mod` 6)))]))
        
-- | Defines turnN for all legitimate parameters, given the next state
turnNDef :: AntState -> Code
turnNDef st = combineList [turnN n st | n <- [-3..3]]

-- | Turns n clockWise (or -n counter-clockwise if n is negative) and then move to a given state
turnN :: Int -> AntState -> Code
turnN n st | n > 0     = define "turnN" [mkParam n, mkParam st] $ turn R (call "turnN" [mkParam (n - 1), mkParam st])
           | n < 0     = define "turnN" [mkParam n, mkParam st] $ turn L (call "turnN" [mkParam (n + 1), mkParam st])
           | otherwise = addLabel "turnN" [mkParam 0, mkParam st] st

        
{-
 - This are the positions that will be found.
 - - => home tile.
 - . => not home tile.
 - M => marker
 -
 -    - - 6 .
 -   - - 2 3 .
 -  - - - 1 M .
 -   - - 4 5 .
 -    - - 7 .
 - 
 - This is the formation the ant will get in by moving ants 2, 3, 4 and 5.
 -    - - 6 .
 -   - - - 2 3
 -  - - - 1 M .
 -   - - - 4 5
 -    - - 7 .
 -
 -}
defendHome :: Code
defendHome = annotate "defendHome" $ combine
    -- Find most right ant and place marker, all other ants go to step1.
    (sense Ahead (jump "step1") next Home)
    (sense LeftAhead (jump "step1") next Home)
    (sense RightAhead (jump "step1") next Home)
    (mark 4 (jump "start"))
    -- Find ant at position 1, mark that position and change state of that ant
    -- to guardEntrance''. All other ants go to step2.
    (annotate "step1" $ combine
        (combineList $ replicate 4 (pickUp next next))  -- Wait untill the first marker is dropped
        (sense Ahead next (jump "step2") (Marker 4))  
        (mark 4 next)
        (combineList $ replicate 4 (pickUp next next))
        (sense LeftAhead (jump "guardEntrance''") this Friend)
    )
    -- Find ants at the right border of the home and in the row above the
    -- markers (position 3). Change the state of that ant to guardEntrance
    -- (RightAhead). All other ants go to step4.
    (annotate "step2" $ combine
        (sense Ahead (jump "step4") next Home)
        (sense RightAhead next (jump "step3") (Marker 4))
        (move next next)
        (pickUp this this)
    )
    -- Find ants at the right border of the home and in the row below the
    -- markers (position 5). Change the state of that ant to guardEntrance
    -- (RightAhead). All other ants go to repositionFoodInit.
    (annotate "step3" $ combine
        --(sense LeftAhead next (jump "repositionFoodInit") (Marker 4))
        --(sense LeftAhead next (jump "start") (Marker 4))
        (sense LeftAhead (relative 23) next (Marker 4))
        -- lock ants at positions 6 and 7.
        (combineList $ replicate 7 (senseMarker))
        (turn L (jump "start"))
        (move (call "guardEntrance" [mkParam LeftAhead]) next)     
    )
    -- Ants not at the right border of the home.
    -- Find ant at position 2 and set its state to guardEntrance' (Ahead).
    -- All other ants go to step5.
    (annotate "step4" $ combine
        (combineList $ replicate 3 (pickUp next next)) -- Wait to make sure the second marker has been droppd
        (sense RightAhead next (jump "step5") (Marker 4))
        (combineList $ replicate 4 (pickUp next next))        
        (mark 4 next)
        (move next next)
        (pickUp this this)
    )
    -- Find ant at position 4 and set its state to guardEntrance' (Ahead).
    -- All other ants go to step5.
    (annotate "step5" $ combine
        --(sense LeftAhead next (jump "repositionFoodInit") (Marker 4))
        (sense LeftAhead next (jump "start") (Marker 4))
        (mark 4 next)
        (move (call "guardEntrance'" [mkParam Ahead]) (jump "start"))
    )
         where senseMarker :: Code
               senseMarker = combine
                (turn L next)
                (sense LeftAhead next (relative 2) (Marker 4))
                (turn R this)

-- For the two ants at the front of the defending formation.
-- Positions 3 and 5.
guardEntrance :: SenseDir -> Code
guardEntrance s = define "guardEntrance" [mkParam s] $ combine
    (sense s next this Foe)
    (combineList $ replicate 24 (turn L next))
    (move (jump "succesFullMove") next)
    -- failedMove
    (combineList $ replicate 5 (turn L next))
    (combineList $ replicate 32 (dropFood next))  -- useless move, just to synchronise with other guards
    (turn L (call "guardEntrance" [mkParam s]))
    -- succesFullMove
    (annotate "succesFullMove" (combineList $ replicate 3 (turn L next)))
    (move next this)
    (combineList $ replicate 2 (turn L next))
    (turn L (call "guardEntrance" [mkParam s]))

-- For the two ants behind the two frontal ants
guardEntrance' :: SenseDir -> Code
guardEntrance' s = define "guardEntrance'" [mkParam s] $ combine
    (sense s this next Friend)
    (move next (call "guardEntrance'" [mkParam s]))
    (combineList $ replicate 3 (turn L next))
    (move next this)
    (combineList $ replicate 2 (turn L next))
    (turn L (call "guardEntrance'" [mkParam s]))

guardEntrance'' :: Code
guardEntrance'' = annotate "guardEntrance''" $ combine
    (combineList $ replicate 4 (pickUp next next))
    (annotate "attack" (sense LeftAhead (jump "makeRoom") next Friend))
    (move (relative 4) next)
    -- move is not possible, only rotate back, don't move back
    (combineList $ replicate 2 (turn L next))
    (turn L (relative 5))
    -- move was possible, rotate and move back
    (combineList $ replicate 3 (turn L next))
    (move next this)
    --
    (combineList $ replicate 3 (turn L next))
    --
    (annotate "makeRoom" (sense Ahead next (jump "attack") Friend))
    (combineList $ replicate 4 (pickUp next next))
    (combineList $ replicate 3 (turn L next))
    (move (jump "start") (jump "start"))

repositionFoodInit :: Code
repositionFoodInit = annotate "repositionFoodInit" $ combine
    (combineList $ replicate 5 (pickUp next next))
    (sense Ahead (jump "repositionFood") (jump "start") (Marker 4))

repositionFood :: Code 
repositionFood = annotate "repositionFood" $ combine
    (turn L (jump "rotate"))
    (annotate "obstacle" $ combine
        -- Assume the obstacle is the defending ants
        (turn L next)
        (move next this)
        (move next this)
        (dropFood next)
        (turn R next)
        (move next this)
        (move (jump "rotate") this)
    )
    (annotate "rotate" $ combine
        (sense Ahead next (jump "repositionFood") Home)
        (sense LeftAhead (jump "obstacle") next (Marker 4))
        (pickUp next next)
        (move (jump "rotate") this)
    )

