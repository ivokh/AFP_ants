{-#LANGUAGE FlexibleInstances, ExistentialQuantification, DeriveDataTypeable#-}
module CodeGenerator (Marker, Instruction(..), Condition(..), SenseDir(..), AntState, MoveDir(..), Code, 
    combine, combineList, runCode, next, this, annotate, define, addLabel, sense, mark, unMark, pickUp, dropFood, turn, move, toss, call, jump, relative, mkParam) where

import Data.Maybe
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

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
 deriving (Show, Eq, Typeable)
 
data Condition = Friend | Foe | FriendWithFood | FoeWithFood | Food | Rock | Marker Marker | FoeMarker | Home | FoeHome 
   deriving (Eq)

instance Show Condition where
    show (Marker i) = "Marker " ++ (show i)
    show Friend = "Friend"
    show Foe = "Foe"
    show FriendWithFood = "FriendWithFood"
    show FoeWithFood = "FoeWithFood"
    show Food = "Food"
    show Rock = "Rock"
    show FoeMarker = "FoeMarker"
    show Home = "Home"
    show FoeHome = "FoeHome"

data SenseDir =
    Here
    | Ahead
    | LeftAhead
    | RightAhead
    deriving (Show, Eq, Typeable)
    
data MoveDir = L | R
    deriving Eq

instance Show MoveDir where
    show L = "Left"
    show R = "Right"

data AntState = Call Function -- Calls a function
                | Relative Int -- Relative n increases the state pointer by n steps
    deriving (Show, Eq, Typeable)
    
data Function = Function String [Param]
    deriving (Show, Eq, Typeable)
    
-- | Data type for parameters
data Param = forall a . (Eq a, Show a, Typeable a) => Param a
    
instance Show Param where
    show (Param x) = show x ++ " :: " ++ show (typeOf x)
    
instance Eq Param where
    Param x == Param y = show x == show y && typeOf x == typeOf y
    
mkParam :: (Eq a, Show a, Typeable a) => a -> Param
mkParam = Param

-- | The environement, containing function calls tupled with the row number where they are defined
type Env = [(Function, Int)]

type Code' a = ReaderT Env (State (Int, Env)) a

-- | The Code monad produces instructions, using the current environment and antstate index, and builds the environment while doing so
type Code = Code' [Instruction]

-- | We can use combine to combine functions, i.env. concatenate code
class Combine a where
    combine :: Code -> a
    
instance Combine Code where
    combine = id
    
instance Combine a => Combine (Code -> a) where
    combine c1 c2 = combine $ (++) <$> c1 <*> c2
    
-- | A function for combining code stored in a list
combineList :: [Code] -> Code
combineList = foldr combine (return [])
    
-- | Calls a function with parameters
call :: String -> [Param] -> AntState
call s args = Call (Function s args)

-- | Jumps to a function that doesn't have parameters
jump :: String -> AntState
jump s = Call (Function s [])

-- | relative n moves n rows downwards in the list of instructions
relative :: Int -> AntState
relative = Relative
   
runCode :: Code -> [Instruction]
runCode c = let (instr, (_, env)) = runState (runReaderT c env) (0, []) in instr

-- | Represents the next Instruction
next :: AntState
next = Relative 1

-- | Represents this Instruction
this :: AntState
this = Relative 0
    
-- | Looks up an AntState in the environment
lookup' :: AntState -> Code' Int
lookup' st = 
    do
        (index, _) <- get
        env <- ask
        case st of
            Relative n -> return (index + n)
            Call f     -> return (fromJust' $ lookup f env)
                where fromJust' (Just x) = x
                      fromJust' Nothing = error ("\r\nCould not find: \r\n" ++ show f ++ "\r\nin the environment!")

-- | Lifts instructions to code and also updates the index
return' :: Instruction -> Code
return' instr = ReaderT $ \r ->
    do
        (index, table) <- get
        put (index + 1, table)
        return [instr]

-- | Labels a function without parameters
annotate :: String -> Code -> Code
annotate s code = 
    do
        (index, table) <- get
        put (index, (Function s [], index):table)
        instr <- code
        return instr

-- | Labels a function with parameters
define :: String -> [Param] -> Code -> Code
define s args code = 
    do
        (index, table) <- get
        put (index, (Function s args, index):table)
        instr <- code
        return instr

-- | Adds an additional label to a given antstate
addLabel :: String -> [Param] -> AntState -> Code
addLabel name args st =
    do
        loc <- lookup' st
        (index, table) <- get
        put (index, (Function name args, loc) : table)
        return []

-- | Go to Code st1 if cond holds in sensedir; and to Code st2 otherwise.
sense :: SenseDir -> AntState -> AntState -> Condition -> Code
sense sensedir st1 st2 cond = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Sense sensedir st1' st2' cond

-- | Set mark i in current cell and go to st.
mark :: Marker -> AntState -> Code
mark n st = 
    do
        st' <- lookup' st
        return' $ Mark n st'

-- | Clear mark i in current cell and go to st.
unMark :: Marker -> AntState -> Code
unMark n st = 
    do
        st' <- lookup' st
        return' $ Unmark n st'

-- | Pick up food from current cell and go to st1 ; go to st2 if there is no food in the current cell.
pickUp :: AntState -> AntState -> Code
pickUp st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ PickUp st1' st2'

-- | Drop food in current cell and go to st.
dropFood :: AntState -> Code
dropFood st = 
    do
        st' <- lookup' st
        return' $ Drop st'

-- | Turn left or right and go to st.
turn :: MoveDir -> AntState -> Code
turn dir st =  
    do
        st' <- lookup' st
        return' $ Turn dir st'

-- | Move forward and go to st1 ; go to st2 if the cell ahead is blocked.
move :: AntState -> AntState -> Code
move st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Move st1' st2'

-- | Choose a random number x from 0 to p-1 ; go to st1 if x=0 and st2 otherwise.
toss :: Int -> AntState -> AntState -> Code
toss n st1 st2 = 
    do
        st1' <- lookup' st1
        st2' <- lookup' st2
        return' $ Flip n st1' st2'