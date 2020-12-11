{-
   Challenge Day 10: ugh combinatorics ugh.
-}
module Adapters where

import Data.Sort
import Data.List

{-
   Generate the list of differences of contiguous elements
   i.e. [(x(i+1) - xi)] for all i
-}
differences :: [Int] -> [Int]
differences l =
    zipWith (-) (tail l) l

{-
   Given a list of numbers, count the number of occurrences
   of 1s, 2s and 3s.
-}
count123 :: [Int] -> (Int,Int,Int)
count123 =
    foldl count0 (0,0,0)
    where count0 (a1,a2,a3) 1 = (a1+1,a2  ,a3  )
          count0 (a1,a2,a3) 2 = (a1  ,a2+1,a3  )
          count0 (a1,a2,a3) 3 = (a1  ,a2  ,a3+1)
          count0 a _ = a

{-
   Take a list of numbers l, sort it increasingly, append 0
   at the beginning and max(l) + 3 at the end.
-}
setup :: [Int] -> [Int]
setup l =
    l' ++ [max + 3]
    where l' = 0:(sort l)
          max = (last l')

{-
   Perform the counting (solution of part 1)
-}
countChain :: [Int] -> (Int,Int,Int)
countChain =
    count123 . differences . setup

{-
   For part 2: it is not even remotely possible to work
   on the complete sequence. Fortunately, in fact, only parts
   of this sequence is able to move.

   A sequence s is valid iff s(n+1) - s(n) <= 3.
   A valid transformation of a sequence consists in removing
   some s(i) such that, if the input sequence is valid, then
   the resulting sequence is valid as well.

   There are numbers in the sequence that are never touched by
   any valid transformation: in particular, any numbers
   (s(i),s(i+1)) such that (s(i+1)-s(i) == 3) are never touched;
   if you remove s(i) or s(i+1), then the resulting sequence
   cannot be valid!
   This allows to identify "non-invariant subsequences", i.e.
   parts of a sequence that can be safely modified.
   For instance, in [1,2,3,6,7,8], we have two non-invariant
   subsequences : [1,2,3] and [6,7,8].

   Determining combination of a sequence is determining the
   combinations of every non-invariant subsequences and put 
   them together.
-}

{-
   subchains takes a sorted list of numbers and generates a list
   of non-invariant subsequences.

   Complexity: O(n), where n is the list's size.
-}
subchains :: [Int] -> [[Int]]
subchains [] = []
subchains [x] = [[x]]
subchains (x1:x2:xs)
    | x2 - x1 >= 3 = [x1]:(subchains (x2:xs))
    | otherwise =
        case subchains (x2:xs) of
          [] -> [[x1]]
          (y:ys) -> (x1:y):ys

{-
   Generate any possible valid chain between start and end
   with the given maximal chain (by removing selected numbers).
-}
chains :: Int -> Int -> [Int] -> [[Int]]
chains end start []
    | start == end = [[end]]
    | otherwise = []
chains end start (x:xs)
    | x - start > 3 = []
    | otherwise = (map (start:) $ chains end x xs) ++ (chains end start xs)

{-
   From a non-invariant subsequence, generate every valid chain
-}
reduce :: [Int] -> [[Int]]
reduce [] = []
reduce [x] = [[x]]
reduce [x,y] = [[x,y]]
reduce l =
    chains (last l) (head l) (tail l)

{-
   Given a list of non-invariant sequences, generate any valid
   transformation of the overal sequence.
-}
reduceAll :: [[Int]] -> [[[Int]]]
reduceAll [] = [[]]
reduceAll (x:xs) =
    case x of
      []    -> reduceAll xs
      [_]   -> map (x:) $ reduceAll xs
      [_,_] -> map (x:) $ reduceAll xs
      _     -> (map (:) $ reduce x) <*> reduceAll xs

{-
   This algorithm above "works" (not thoroughly tested but
   should be fine), but there is a "slight" problem...
   There might be a very, very large number of combinations...
   Larger than the amount of memory on any machine, actually.

   Fortunately, we only need to *count* the number of combination.
   The idea here is to simply turn the actual generation into
   a combination calculus... Which is fairly easy, in fact.
-}

{-
   Count the number of combinations of valid transformation of
   the given subsequence.
-}
reduce' :: [Int] -> Int
reduce' [] = 0
reduce' [x] = 1
reduce' [x,y] = 1
reduce' l = length $ chains (last l) (head l) (tail l)

{-
   Count the total number of combinations for a list of
   subsequences. In fact, if the last blocks has m combinations,
   and the block just before has n combinations, then, those
   blocks taken together have a total of n*m combinations.
-}
reduceAll' :: [[Int]] -> Int
reduceAll' [] = 1
reduceAll' (x:xs) =
    case x of
      []    -> reduceAll' xs
      [_]   -> reduceAll' xs
      [_,_] -> reduceAll' xs
      _     -> (reduce' x) * (reduceAll' xs)





