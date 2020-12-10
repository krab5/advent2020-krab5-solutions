module Main where

import Control.Monad
import Toboggan

{-
   For each slope and given a grid, count the number of
   trees on the path of the toboggan, print it and return
   it.
-}
process :: [[Char]] -> (Int,Int) -> IO Int
process grid slope =
    let n = countTrees slope grid in do
        putStrLn $ "Number of trees for slope " ++ show slope ++ ": " ++ show n
        return n

file = "forest.txt"
slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- Main function
main :: IO ()
main = do
    ct <- readFile file
    grid <- return $ megacycle $ lines ct -- Create the grid
    ls <- forM slopes $ process grid      -- Process all slopes and get a list of numbers
    putStrLn $ "Product:" ++ (show $ foldl (*) 1 ls) -- Product of numbers





