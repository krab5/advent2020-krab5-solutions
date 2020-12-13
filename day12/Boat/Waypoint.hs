{-
   An executer to address part 2 of the challenge
-}
module Boat.Waypoint (
        WState,
        coord,
        show,
        waypointExecuter
    ) where

import Boat.Exec
import Boat.Coord
import Boat.Instruction

data WState = WState {
        waypoint :: Coord,
        coord :: Coord
    }

instance Show WState where
  show wst = "Boat at " ++ (show $ coord wst) ++ " going to " ++ (show $ waypoint wst)

_initialState :: WState
_initialState = WState { waypoint = (10,1), coord = (0,0) }

getNumFromAngle :: Int -> Int
getNumFromAngle n = (n `div` 90) `mod` 4

_doRotate :: Direction -> Int -> WState -> WState
_doRotate dir angle wst =
    wst { waypoint = rot dir angle $ waypoint wst }
    where rot DLeft = rotateCCW
          rot DRight = rotateCW

_goCard :: Cardinal -> Int -> WState -> WState
_goCard card n wst =
    wst { waypoint = (waypoint wst) @+ (trans card n) }
    where trans North = north
          trans East  = east
          trans South = south
          trans West  = west

_goForward :: Int -> WState -> WState
_goForward 0 wst = wst
_goForward n wst =
    _goForward (n - 1) $ step wst
    where step wst = wst { coord = (coord wst) @+ (waypoint wst) }

waypointExecuter :: Executer WState
waypointExecuter = Executer {
        doRotate = _doRotate,
        goCard = _goCard,
        goForward = _goForward,
        initialState = _initialState,
        distance = (manhattan (coord _initialState)) . coord
    }




