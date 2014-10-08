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

instance Show MoveDir where
    show L = "Left"
    show R = "Right"

type Env = [(AntState, Int)]

type Code = State (Int, Env) [Instruction]

-- | We can use combine to combine functions, i.env. concatenate code
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
lookup' :: Env -> AntState -> State (Int, [(AntState, Int)]) Int
lookup' env st = 
    do
        (index, _) <- get
        case st of
            Relative n -> return (index + n)
            _           -> return (fromJust $ lookup st env)

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
        (index, table) <- get
        put (index, (Jump s, index):table)
        instr <- code
        return instr

-- | Go to Code st1 if cond holds in sensedir; and to Code st2 otherwise.
sense :: Env -> SenseDir -> AntState -> AntState -> Condition -> Code
sense env sensedir st1 st2 cond = 
    do
        st1' <- lookup' env st1
        st2' <- lookup' env st2
        return' $ Sense sensedir st1' st2' cond

-- | Set mark i in current cell and go to st.
mark :: Env -> Marker -> AntState -> Code
mark env n st = 
    do
        st' <- lookup' env st
        return' $ Mark n st'

-- | Clear mark i in current cell and go to st.
unMark :: Env -> Marker -> AntState -> Code
unMark env n st = 
    do
        st' <- lookup' env st
        return' $ Unmark n st'

-- | Pick up food from current cell and go to st1 ; go to st2 if there is no food in the current cell.
pickUp :: Env -> AntState -> AntState -> Code
pickUp env st1 st2 = 
    do
        st1' <- lookup' env st1
        st2' <- lookup' env st2
        return' $ PickUp st1' st2'

-- | Drop food in current cell and go to st.
dropFood :: Env -> AntState -> Code
dropFood env st = 
    do
        st' <- lookup' env st
        return' $ Drop st'

-- | Turn left or right and go to st.
turn :: Env -> MoveDir -> AntState -> Code
turn env dir st =  
    do
        st' <- lookup' env st
        return' $ Turn dir st'

-- | Move forward and go to st1 ; go to st2 if the cell ahead is blocked.
move :: Env -> AntState -> AntState -> Code
move env st1 st2 = 
    do
        st1' <- lookup' env st1
        st2' <- lookup' env st2
        return' $ Move st1' st2'

-- | Choose a random number x from 0 to p-1 ; go to st1 if x=0 and st2 otherwise.
toss :: Env -> Int -> AntState -> AntState -> Code
toss env n st1 st2 = 
    do
        st1' <- lookup' env st1
        st2' <- lookup' env st2
        return' $ Flip n st1' st2'