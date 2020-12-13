{-
   Abstraction of an execution plan, i.e. something that
   performs the series of transformations given by the a
   list of instructions.

   Executer abstract the semantics of the instruction:
   the user provide the mandatory functions that describe
   the behaviour of each instructions, and the executer blindly
   applies these functions as it parses the input instructions.

   Answering the challenge is done by defining custom executer
   for each interpretation of the instructions.
-}
module Boat.Exec where

import Boat.Coord
import Boat.Instruction

{-
   An executer for a given state consists of a function to go
   a given direction, a function to perform a ortation, and a
   function to go forward (3 types of instructions).

   We add an initial state (for convenience) and a distance from
   the initial state function (not very good to put it here, but,
   oh well).
-}
data Executer s = Executer {
        goCard    :: Cardinal -> Int -> s -> s,
        doRotate  :: Direction -> Int -> s -> s,
        goForward :: Int -> s -> s,
        initialState :: s,
        distance  :: s -> Int
    }

{-
   Execute the given instruction on the given state with
   the given executer.
-}
execInst :: Executer s -> Instruction -> s -> s
execInst ex (Go    c n) = goCard    ex c n
execInst ex (Turn  d n) = doRotate  ex d n
execInst ex (Forward n) = goForward ex n

{-
   Execute a series of instructions with the given executer.
   The executer embeds the initial state.
-}
execAll :: Executer s -> [Instruction] -> s
execAll ex l =
    foldl (flip (execInst ex)) (initialState ex) l


