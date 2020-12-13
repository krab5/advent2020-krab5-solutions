{-
   A simple executer to answer part 1 of the challenge.
-}
module Boat.State (
        State,
        simpleExecuter,
        coord,
        show
    ) where

import Boat.Exec
import Boat.Coord
import Boat.Instruction

data State = State {
        coord :: Coord,
        facing :: Cardinal
    }

instance Show State where
  show st =
      "Boat at " ++ show c ++ ", facing " ++ show fa
      where c = coord st
            fa = facing st

_initialState :: State
_initialState = State { coord = (0,0), facing = East }

_doRotate :: Direction -> Int -> State -> State
_doRotate dir angle st =
    st { facing = rotate dir angle $ facing st }

_goCard :: Cardinal -> Int -> State -> State
_goCard card n st =
    st { coord = (coord st) @+ trans card n }
    where trans North = north
          trans East  = east
          trans South = south
          trans West  = west

_goForward :: Int -> State -> State
_goForward n st =
    _goCard (facing st) n st

simpleExecuter :: Executer State
simpleExecuter = Executer {
        goCard = _goCard,
        doRotate = _doRotate,
        goForward = _goForward,
        initialState = _initialState,
        distance = (manhattan (coord _initialState)) . coord
    }


