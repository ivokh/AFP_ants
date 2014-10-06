data Marker = Zero

data SenseDir =
    Here
    | Ahead
    | LeftAhead
    | RightAhead

data MoveDir = Left | Right

data State = State [String]

-- Go to state st1 if cond holds in sensedir; and to state st2 otherwise.
sense :: SenseDir -> State -> State -> Marker -> State
sense = undefined

-- Set mark i in current cell and go to st.
mark :: Marker -> State -> State
mark = undefined

-- Clear mark i in current cell and go to st.
unMark :: Marker -> State -> State
unMark = undefined

-- Pick up food from current cell and go to st1 ; go to st2 if there is no food in the current cell.
pickUp :: State -> State -> State
pickUp = undefined

-- Drop food in current cell and go to st.
drop :: State -> State
drop = undefined

-- Turn left or right and go to st.
turn :: MoveDir -> State -> State
turn = undefined

-- Move forward and go to st1 ; go to st2 if the cell ahead is blocked.
move :: State -> State -> State
move = undefined

-- Choose a random number x from 0 to p-1 ; go to st1 if x=0 and st2 otherwise.
flip :: Int -> State -> State -> State
flip = undefined
