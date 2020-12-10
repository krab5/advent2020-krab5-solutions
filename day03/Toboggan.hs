{-
   Challenge Day 3: count the number of "trees" in a "map",
   represented as a grid of dots and hashes (the trees being
   hashes) for various slopes.

   The map is supposed to cycle on its right. We simply use
   infinite list to do that (we could have used vectors and
   rotating indices but what's the fun in that).

   Moving on the grid consists in trimming the list of
   infinite lists (to go down) and trimming each infinite
   list (to go right).
-}
module Toboggan where

import Control.Monad

{-
   Build a list of infinite lists from a list of lists
-}
megacycle :: [[a]] -> [[a]]
megacycle = map cycle

-- This represents a tree
tree = '#'

{-
   Do the counting for a given slope and a given grid.
   The idea is to check the character in the top left (which
   is basically (head $ head l)) and add it to the count,
   then "move" the grid by trimming it.

   The function is terminal recursive (hurray!).
-}
countTrees :: (Int,Int) -> [[Char]] -> Int
countTrees slope grid =
    countTrees1 0 grid
    where move (r,d) = (map $ drop r) . (drop d)
          checkTree x = if x == tree then 1 else 0
          countTrees1 count [] = count
          countTrees1 count grid@((x:_):_) =
              countTrees1 (count + (checkTree x)) $ move slope grid




