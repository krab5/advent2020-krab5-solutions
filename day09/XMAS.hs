{-
   Challenge Day 9: more combinatorics!

   The first part is easy; the second part is interesting.
   I decided to propose two solutions: a first "brute force"
   solution that exploits the list monad in a neat way, and
   another more subtle taking advantage of the puzzle's
   constraints.
-}
module XMAS where

import Data.List
import Text.Read
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad

{-
   Find a pair of number in a list such that their sum
   is equal to the given target (n).

   We use the list monad, which is kind of like constraint
   programming; no sweat!
-}
findPair :: Int -> [Int] -> [(Int,Int)]
findPair n nums = do
    x <- nums
    y <- nums
    guard $ x /= y
    guard $ x + y == n
    return (x,y)

{-
   Compute every subsequences of the given list.
   A subsequence is any list of contiguous elements of the
   given list.
   E.g.
   > [1,2,3] -> [[1],[1,2],[1,2,3],[2],[2,3],[3]]
   Note that this is different from combinations as we want
   to preserve the order (to avoid aving similar sequences)
-}
ordSubsequences :: [Int] -> [[Int]]
ordSubsequences [] = []
ordSubsequences l@(_:xs) =
    (drop 1 $ inits l) ++ (ordSubsequences xs)

{-
   Find the wanted subsequence using the list monad!
   This is very fun but terribly inefficient as there are absolutely
   no heuristic here...
-}
findSubSeq :: Int -> [Int] -> Maybe [Int]
findSubSeq n nums =
    case findit of
      [] -> Nothing
      (x:_) -> Just x
    where findit = do
            xs <- ordSubsequences nums
            guard $ length xs > 1
            guard $ sum xs == n
            return xs

{-
   Create a sequence of overlapping windows of given size
   (such that each windows is offset by 1 element).
   E.g.:
   > windows 2 [1,2,3] -> [[1,2],[2,3]]
-}
windows :: Int -> [Int] -> [[Int]]
windows size nums
    | length nums < size = []
    | (_:xs) <- nums = (take size nums):(windows size xs)
    | otherwise = []

{-
   Associate each number of the given list, beyond the element
   of index "size", to the window of size "size" that preceds it
   E.g.
   > separate 2 [1,2,3,4] = [(3,[1,2]),(4,[2,3])]
-}
separate :: Int -> [Int] -> [(Int,[Int])]
separate size nums =
    zip rem win
    where win = windows size nums
          rem = drop size nums

{-
   Check for each (n,l) that there exists two numbers in l
   such that their sum is n.
-}
check :: [(Int,[Int])] -> [Bool]
check =
    (map (not . null)) . (map (uncurry findPair))

{-
   Check for each number of the list that there exists two
   number in the window of size "size" such that their sum
   is equal to that number.
-}
nom :: Int -> [Int] -> [(Bool,Int)]
nom size nums =
    zipWith comb sep ch
    where sep = separate size nums
          ch  = check sep
          comb (n,_) b = (b,n)

{-
   Find the first element that validates the given predicate.
-}
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst p l =
    case dropWhile (not . p) l of
      []    -> Nothing
      (x:_) -> Just x



-- ALTERNATE VERSION
{-
   Get a slice of vector between the bounds l and r inclusive.
   (note: the original slice takes the lower bound and the
   slice's size).
-}
slice' :: (Int,Int) -> V.Vector a -> V.Vector a
slice' (l,r) v = V.slice l (r - l + 1) v

{-
   Sum the numbers in a given vector, in the given
   range.
-}
sumSlice :: Num a => (Int,Int) -> V.Vector a -> a
sumSlice ran v = V.sum $ slice' ran v

{-
   Find a range (i,j) of the given vector such its sum is equal to the target.

   This is done using a "caterpilar method". Given t the target and (i,j) the
   current range:
   - if sum(i,j) is lower than t, then retry with sum(i,j+1)
   - if sum(i,j) is greater than t, then retry with sum(i+1,j)
   - if i == j, retry with sum(i+1,+2)
   - if j > |vector| then there is no solution
   - if sum(i,j) == t then we are done!

   This algorithm works because excluding a number decreases the sum and 
   including a number increases it. A strong precondition for that is that
   the sequence of numbers is positive...

   Also, we are looking for subsequences; without the contiguity of values,
   this wouldn't work.
-}
findSubSeq1 :: (Num a, Eq a, Ord a) => a -> V.Vector a -> Maybe (Int,Int)
findSubSeq1 target vec =
    increaseTo (0,1)
    where n = V.length vec
          increaseTo (i,j)
              | j >= n = Nothing
              | otherwise =
                  let s = sumSlice (i,j) vec in
                      if s == target
                          then Just (i,j)
                      else if s > target
                          then decreaseTo (i+1,j)
                      else
                          increaseTo (i,j+1)
          decreaseTo (i,j)
              | j - i < 1 = increaseTo (i+1,i+2)
              | otherwise =
                  let s = sumSlice (i,j) vec in
                      if s == target
                          then Just (i,j)
                      else if s < target
                          then increaseTo (i,j+1)
                      else
                          decreaseTo (i+1,j)

{-
   Wrap findSubSeq1 nicely to be used (relatively) directly in the program
-}
findSubSeq2 :: (Num a, Eq a, Ord a) => a -> V.Vector a -> Maybe (V.Vector a)
findSubSeq2 target vec = do
    ran <- findSubSeq1 target vec
    return $ slice' ran vec






