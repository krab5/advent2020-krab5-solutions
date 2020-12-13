{-
   Challenge Day 11: cellular automaton.
   Simulate a set of rules + initial conditions until it
   converges.
-}
module Main where

import Seating.Seat
import Seating.Stage
import Seating.Rule
import qualified Data.Vector as V

{-
   Count the number of elements satisfying the given predicate
   in the given foldable collection.
-}
count :: Foldable t => (a -> Bool) -> t a -> Int
count p =
    foldl (\acc x -> if p x then acc + 1 else acc) 0

{-
   Rule for part 1: you don't seet on the floor, if every neighboring seat
   is vacant then you take the seat, if there are at least 4 occupied
   neighboring seat, you leave it.
-}
rule :: Rule
rule Floor    _ = Nothing
rule Vacant   l
    | all isFree l = Just Occupied
    | otherwise = Nothing
rule Occupied l
    | count (not . isFree) l >= 4 = Just Vacant
    | otherwise = Nothing

{-
   Rule for part 2: same as for part 1 except you leave if there are
   5 occupied seats.
-}
rule2 :: Rule
rule2 Floor    _ = Nothing
rule2 Vacant   l
    | all isFree l = Just Occupied
    | otherwise = Nothing
rule2 Occupied l
    | count (not . isFree) l >= 5 = Just Vacant
    | otherwise = Nothing

file = "seats.txt"

{-
   Run a model until it converges and show the results.
-}
process :: Neighborhood -> Rule -> String -> Stage Seat -> IO ()
process neighbors rule text st = do
    putStrLn $ "== Running model " ++ text
    let (n,st') = updateTilFix neighbors rule st in do
        putStrLn $ "Convergence met in " ++ show n ++ " steps."
        putStrLn $ "Number of seats occupied: " ++ (show $ count (== Occupied) (content st'))
        putStrLn "Result:"
        putStrLn $ show st'

{-
   Main function; runs both models.
-}
main :: IO ()
main = do
    st <- makeStage <$> lines <$> readFile file
    process neighbors rule "Part1" st
    process farNeighbors rule2 "Part2" st




