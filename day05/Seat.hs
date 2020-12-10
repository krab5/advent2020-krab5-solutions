{-
   Challenge Day 5: guess your seat given a number of
   rules and constraints.
-}
module Seat where

import Data.List
import Data.Maybe

-- F = Lo, B = Up
-- L = Lo, R = Up

{-
   This model a general zone: either front or back,
   or left and right.
   Front and left are considered "Lo" (low) because they
   are in the lower part of the range, and back and right
   are considered "Up" because they are in the upper part
   of the range.
-}
data Zone = Up | Lo deriving (Show,Read,Enum,Eq)

{-
   Given a range of numbers and a zone, calculated the
   next range of number (by keeping the relevant part of
   that range).
-}
zone :: (Int,Int) -> Zone -> (Int,Int)
zone (lo,hi) Lo = (lo,hi - 1 - (hi - lo) `div` 2)
zone (lo,hi) Up = (lo + 1 + (hi - lo) `div` 2,hi)

{-
   Get a single number (hopefully) from a starting range and
   a list of zones.
   Note that if the resulting range has two numbers (which
   _shouldn't_ happen), the lower bound is taken.
-}
getzone :: (Int,Int) -> [Zone] -> Int
getzone range zones =
    let (lo,hi) = foldl zone range zones in
        lo

{-
   Transform a front/back specifier into a zone.
-}
row :: Char -> Zone
row 'F' = Lo
row 'B' = Up

{-
   Transform a left/right specifier into a zone.
-}
col :: Char -> Zone
col 'L' = Lo
col 'R' = Up

{-
   Parse a seat specification into two list of zones.
   The first 7 letters specify the row (front/back) and the
   3 remaining ones specify the column (left/right).
-}
parse :: String -> ([Zone],[Zone])
parse str =
    let (srow,scol) = splitAt 7 str in
        (map row srow,map col scol)

-- Actual ranges for this problem
rowrange = (0,127)
colrange = (0,7)

{-
   Given a seat specifier, guess the corresponding seat
   as a (row,column) pair.
-}
guessSeat :: String -> (Int,Int)
guessSeat str =
    (theRow,theCol)
    where (rp,cp) = parse str
          theRow = getzone rowrange rp
          theCol = getzone colrange cp

{-
   Calculate the seat idea of a seat as (row,column).
-}
seatID :: (Int,Int) -> Int
seatID (ro,co) = ro * 8 + co

{-
   Detect a discrepency in a list of seats.
   The list of seats is sorted, and we check that any contiguous
   numbers in the list are continguous, i.e. the list is of the form
   > [n,n+1,n+2,...]
   We report every discrepency, although their should be only one.
-}
detectFaults :: [Int] -> [(Int,Int)]
detectFaults l =
    -- The main idea is to turn a list [a,b,c,..] into a list
    -- [(a,b,b-a),(b,c,c-b),...], i.e. a list of the current value, the next
    -- value, and the difference between these two values.
    -- The only thing left to do is to check when the given difference is not 1
    -- and report it.
    map fromJust $ filter isJust $ map check $ zipWith (\a b -> (a,b,b - a)) l' (tail l')
    where l' = sort l
          check (_,_,1) = Nothing
          check (a,b,_) = Just (a,b)





