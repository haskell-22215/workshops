module State where

import Control.Monad.State

-- put
-- get, gets
-- modify

-- runState, evalState, execState

data Turtle = Turtle { x :: Int, y :: Int } deriving (Eq, Show)

data TurtleAction = North | West | East | South | NorthWest | NorthEast | SouthWest | SouthEast deriving (Eq, Show)

runTurtle :: TurtleAction -> State Turtle ()
runTurtle action =
  case action of
    North -> moveTurtleUp
    West -> moveTurtleLeft
    East -> moveTurtleRight
    South -> moveTurtleDown
    NorthWest -> moveTurtleUp >> moveTurtleLeft
    NorthEast -> do
      moveTurtleUp
      moveTurtleRight
    SouthWest -> (moveTurtleDown >>= \_ -> moveTurtleLeft)
    SouthEast -> moveTurtleDown >> moveTurtleRight

runTurtleExample = do
  runTurtle NorthWest
  runTurtle West
  runTurtle South

moveTurtleUp :: State Turtle ()
moveTurtleUp = do
  s <- get
  let y' = y s - 1
  put $ s { y = y' }

moveTurtleLeft :: State Turtle ()
moveTurtleLeft = modify (\s -> s { x = (x s) - 1 })

moveTurtleRight :: State Turtle ()
moveTurtleRight = modify (\s -> s { x = (x s) + 1 })

moveTurtleDown :: State Turtle ()
moveTurtleDown = modify (\s -> s { y = (y s) + 1 })

type Stack a = [a]

emptyStack :: Stack a
emptyStack = []

push :: Stack a -> a -> Stack a
st `push` v = v : st

pop :: Stack a -> a
pop = head

data RpnAction = Const Int | Plus | Minus deriving (Eq, Show)

data Tree a = Leaf a | Branch (Tree a) a (Tree a) deriving (Eq, Show)

