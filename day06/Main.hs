module Main where

import Custom

file = "answers.txt"

main :: IO ()
main = do
    bks <- (blocks . lines) <$> readFile file
    putStrLn $ "Sum: " ++ show (countAll $ map aggregate bks)




