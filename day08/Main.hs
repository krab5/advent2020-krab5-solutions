{-
   Challenge Day 8: some kind of micro assembly language
   execution and finding a mutation that prevent infinite
   loops.

   For this challenge I actually wrote an execution engine
   (very simple, using the State monad) and a "mutator" to
   generate single instruction mutations of the input
   program (kind of brute-force then...).
-}
module Main where

import Data.List
import qualified Data.Vector as V
import Boot.Inst
import Boot.Exec
import Boot.Parse
import Boot.Mutator
import Control.Monad.State
import Control.Monad

{-
   Generate a bunch of mutation of a list of instructions and run
   them, filtering out those that have stopped "naturally", and
   excluding those for which we detect an infinite loop.
-}
fixProgram :: Insts -> [(Int,Registers)]
fixProgram insts =
    map (snd <$>) $ filter check allRuns
    where mutants = mutate insts
          mutantStates = map (runUntilStop <$>) mutants
          allRuns = map (fromStart <$>) mutantStates
          check (_, (Stopped, reg)) = True
          check _ = False


file = "boot.txt"

main :: IO ()
main = do
    insts <- parseFile file
    let sta = runUntilStop insts in
        let (st, reg) = fromStart sta in do
            putStrLn $ "Program terminated because of " ++ show st
            putStrLn "Result of execution:"
            putStrLn $ show reg
    putStrLn "Looking for fixing mutation"
    forM_ (fixProgram insts) $ \(n,r) -> do
        putStrLn "Valid mutation found!"
        putStrLn $ "Altering line " ++ (show $ n + 1) ++ " (" ++ (show $ V.unsafeIndex insts n) ++ "):"
        putStrLn $ "=> Result: " ++ show r





