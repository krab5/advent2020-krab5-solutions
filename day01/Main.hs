module Main where

import Data.Sort
import SumProd

-- Target number
target = 2020
-- Input file (puzzle input)
file = "sumprod_input.txt"

{-
   main function
   Reads the file (file), retrieves the list of numbers, find the
   requested pair and then the requested triplet.
   Print the found solution and the product of its components (as 
   requested by the puzzle).
-}
main :: IO ()
main = do
    ls <- parseAll
    case findPair (sort ls) target of
        Nothing -> putStrLn "No result found"
        Just (x,y) -> putStrLn $ "Result found: " ++ show (x,y) ++ ", product: " ++ show (x*y)
    case findTriplet (sort ls) target of
        Nothing -> putStrLn "No result found"
        Just (x,y,z) -> putStrLn $ "Result found: " ++ show (x,y,z) ++ ", product: " ++ show (x*y*z)
    where parseAll :: IO [Int]
          parseAll = readFile file >>= (\ct -> return $ map read $ lines ct)







