import Control.Monad.State
import Data.Maybe

type AntState = Int

data Instruction 
   = Sense SenseDir AntState AntState Condition
   | Mark Marker AntState
   | Unmark Marker AntState
   | PickUp AntState AntState
   | Drop AntState
   | Turn MoveDir AntState
   | Move AntState AntState
   | Flip Int AntState AntState
 deriving Show
 
data Condition = Friend | Foe | FriendWithFood | FoeWithFood | Food | Rock | Marker Marker | FoeMarker | Home | FoeHome 
   deriving Show
   
type Marker = Int

data SenseDir =
    Here
    | Ahead
    | LeftAhead
    | RightAhead
    deriving Show

data AntJob = CollectFood AntJob | Attack AntJob | Loop
    deriving Eq

data MoveDir = Left | Right
    deriving Show

type Code = State (Int, [(AntJob, Int)]) [Instruction]

-- Looks up an antjob in the environment
lookup' :: AntJob -> State (Int, [(AntJob, Int)]) Int
lookup' job = 
    do
        (_, table) <- get
        return (fromJust $ lookup job table)

-- Lifts instructions to code and also updates the index
return' :: [Instruction] -> Code
return' instr =
    do
        (index, table) <- get
        put (index + length instr, table)
        return instr

-- Labels a computation with an antjob
annotate :: AntJob -> Code -> Code
annotate job code =
    do
        instr <- code
        (index, table) <- get
        put (index, (job, index):table)
        return' instr

-- Go to Code st1 if cond holds in sensedir; and to Code st2 otherwise.
sense :: SenseDir -> AntJob -> AntJob -> Condition -> Code
sense sensedir st1 st2 cond = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' [Sense sensedir st1' st2' cond]

-- Set mark i in current cell and go to st.
mark :: Marker -> Code -> Code
mark = undefined

-- Clear mark i in current cell and go to st.
unMark :: Marker -> Code -> Code
unMark = undefined

-- Pick up food from current cell and go to st1 ; go to st2 if there is no food in the current cell.
pickUp :: Code -> Code -> Code
pickUp = undefined

-- Drop food in current cell and go to st.
drop :: Code -> Code
drop = undefined

-- Turn left or right and go to st.
turn :: MoveDir -> Code -> Code
turn = undefined

-- Move forward and go to st1 ; go to st2 if the cell ahead is blocked.
move :: Code -> Code -> Code
move = undefined

-- Choose a random number x from 0 to p-1 ; go to st1 if x=0 and st2 otherwise.
flip :: Int -> Code -> Code -> Code
flip = undefined

