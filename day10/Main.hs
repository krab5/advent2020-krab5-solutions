module Main where

import Adapters
import Data.Sort

file = "adapters.txt"

main :: IO ()
main = do
    nums <- map read <$> lines <$> readFile file
    (ones,twos,threes) <- return $ countChain nums
    putStrLn $ "Number of 1: " ++ show ones ++ ", 2: " ++ show twos ++ ", 3: " ++ show threes
    putStrLn $ "<1>×<3> = " ++ show (ones*threes)
    -- chs <- return $ reduceAll $ subchains $ setup nums
    -- answer <- return $ length chs
    answer <- return $ reduceAll' $ subchains $ setup nums
    putStrLn $ "Number of combinations: " ++ (show answer)



