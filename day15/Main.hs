{-
   Challenge Day 15: numerical game.
   Realized it with "semi-intelligent brute-forcing".
   The major difficulty with the problem is that we need to
   store up to 30000000 values in memory. The search itself
   is a problem, but space complexity is more important.

   We take advantage of the sparse repartition of the numbers
   to be stored. A simple huge vector would be the most efficient,
   but we cannot decently store 30000000 integers in memory!
   We use a bit tree to store the values; this quite lightweight,
   but even with "relatively fast" search, the algorithm is still
   quite long (more than 10 minutes? didn't time it).
-}
module Main where

import qualified BitTree as B
import System.Environment (getArgs)

{-
   Determine the result of 1 turn: number of the next turn, value
   "said" at that turn, and updated structure.

   The function is generic to be able to provide other "oracle"
   (function to guess the update structure and thing to say).
-}
do1Turn :: (Int -> Int -> a -> (Int,a)) -> (Int, Int, a) -> (Int, Int, a)
do1Turn searchAndUpdate (turn,last,struc) =
    let (last',struc') = searchAndUpdate last turn struc in
        (turn + 1, last', struc')

{-
   Do a series of turns until the last turn played is "target".
-}
doTurn :: (Int -> Int -> a -> (Int,a)) -> Int -> (Int, Int, a) -> (Int, Int, a)
doTurn searchAndUpdate target arg@(turn,_,_)
    | target == turn = arg
    | otherwise = doTurn searchAndUpdate target $ do1Turn searchAndUpdate arg

{-
   Do a series of turns until the last turn played is "target", and
   record every answers said.
-}
doTurn' :: (Int -> Int -> a -> (Int,a)) -> Int -> (Int,[Int],a) -> (Int, [Int], a)
doTurn' searchAndUpdate target arg@(turn,l@(last:_),struc)
    | target == turn = arg
    | otherwise =
        let (turn',last',struc') = do1Turn searchAndUpdate (turn,last,struc) in
            doTurn' searchAndUpdate target (turn',last':l,struc')

{-
   Build the starting triplet.
-}
startB :: [Int] -> Int -> (Int,Int,B.BitTree Int)
startB start _ = (length start, last start, B.makeBTree $ init start)

{-
   Build a starting triplet from the given starting triplet function
   for doTurn'
-}
start' :: ([Int] -> Int -> (Int,Int,a)) -> [Int] -> Int -> (Int,[Int],a)
start' mkStart l t =
    let (turn,_,struct) = mkStart l t in
        (turn,reverse $ l,struct)

{-
   Main function
-}
main :: IO ()
main = do
    (st:tar:_) <- getArgs
    let (start,target) = (read st, read tar) in
        let (_,x,t) = doTurn B.searchAndUpdate target $ startB start target in do
            putStrLn $ "Start: " ++ (show start) ++ " (first turn is number " ++ (show $ length start) ++ ")"
            putStrLn $ "Target: " ++ (show target) ++ "th value"
            putStrLn $ show target ++ "th value: " ++ show x
--            putStrLn $ "Turns:"
--            putStrLn $ show $ reverse l


