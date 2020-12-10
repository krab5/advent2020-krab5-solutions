module Main where

import Passport

file = "passports.txt"

main :: IO ()
main = do
    ct <- lines <$> readFile file
    n <- return $ count isValid $ getPassports ct
    putStrLn $ "Number of 'valid' passports: " ++ show n



