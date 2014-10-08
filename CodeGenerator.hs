{-#LANGUAGE TypeSynonymInstances, FlexibleInstances#-}
module CodeGenerator where

import Control.Monad.State
import Data.Maybe
import Control.Applicative

type Marker = Int

data Instruction 
   = Sense SenseDir Int Int Condition
   | Mark Marker Int
   | Unmark Marker Int
   | PickUp Int Int
   | Drop Int
   | Turn MoveDir Int
   | Move Int Int
   | Flip Int Int Int
 deriving Show
 
data Condition = Friend | Foe | FriendWithFood | FoeWithFood | Food | Rock | Marker Marker | FoeMarker | Home | FoeHome 
   deriving Show
   

data SenseDir =
    Here
    | Ahead
    | LeftAhead
    | RightAhead
    deriving Show

data AntState = Jump String -- Jumps to a function
                | Relative Int -- Relative n increases the state pointer by n steps
    deriving Eq

data MoveDir = L | R
    deriving Show

type Code = State (Int, [(AntState, Int)]) [Instruction]

-- | We can use combine to combine functions, i.e. concatenate code
class Combine a where
    combine :: Code -> a
    
instance Combine Code where
    combine = id
    
instance Combine a => Combine (Code -> a) where
    combine c1 c2 = combine $ (++) <$> c1 <*> c2

-- | Represents the next Instruction
next :: AntState
next = Relative 1
    
-- | Looks up an AntState in the environment
lookup' :: AntState -> State (Int, [(AntState, Int)]) Int
lookup' st = 
    do
        (index, table) <- get
        case st of
            Relative n -> return (index - n)
            _           -> return (fromJust $ lookup st table)

-- | Lifts instructions to code and also updates the index
return' :: Instruction -> Code
return' instr =
    do
        (index, table) <- get
        put (index + 1, table)
        return [instr]

-- | Labels a function
annotate :: String -> Code -> Code
annotate s code =
    do
        instr <- code
        (index, table) <- get
        put (index, (Jump s, index):table)
        return instr

-- Go to Code st1 if cond holds in sensedir; and to Code st2 otherwise.
sense :: SenseDir -> AntState -> AntState -> Condition -> Code
sense sensedir st1 st2 cond = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Sense sensedir st1' st2' cond

-- Set mark i in current cell and go to st.
mark :: Marker -> AntState -> Code
mark n st = 
    do
        st' <- lookup' st
        return' $ Mark n st'

-- Clear mark i in current cell and go to st.
unMark :: Marker -> AntState -> Code
unMark n st = 
    do
        st' <- lookup' st
        return' $ Unmark n st'

-- Pick up food from current cell and go to st1 ; go to st2 if there is no food in the current cell.
pickUp :: AntState -> AntState -> Code
pickUp st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ PickUp st1' st2'

-- Drop food in current cell and go to st.
dropFood :: AntState -> Code
dropFood st = 
    do
        st' <- lookup' st
        return' $ Drop st'

-- Turn left or right and go to st.
turn :: MoveDir -> AntState -> Code
turn dir st =  
    do
        st' <- lookup' st
        return' $ Turn dir st'

-- Move forward and go to st1 ; go to st2 if the cell ahead is blocked.
move :: AntState -> AntState -> Code
move st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Move st1' st2'

-- Choose a random number x from 0 to p-1 ; go to st1 if x=0 and st2 otherwise.
toss :: Int -> AntState -> AntState -> Code
toss n st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Flip n st1' st2'