{-
   Challenge Day 12: determining the result of a sequence of
   transformation in a Cartesian frame.
-}
module Main where

import Boat.Instruction
import Boat.Coord
import Boat.Parser
import Boat.Exec
import qualified Boat.State as S
import qualified Boat.Waypoint as W

file = "instructions.txt"

{-
   Perform the execution of an "executer" (see Boat.Exec)
-}
doExec :: Show s => Executer s -> [Instruction] -> String -> IO ()
doExec exec insts text =
    let st = execAll exec insts in do
        putStrLn $ text ++ " results:"
        putStrLn $ "Final state: " ++ show st
        putStrLn $ "Manhattan distance: " ++ (show $ distance exec st)

main :: IO ()
main = do
    insts <- parseAll <$> readFile file
    doExec S.simpleExecuter insts "Simple Executer"
    doExec W.waypointExecuter insts "Waypoint Executer"


