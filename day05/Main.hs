module Main where

import Seat


file = "passes.txt"

main :: IO ()
main = do
    ls <- lines <$> readFile file
    ids <- return $ map process ls
    ma <- return $ maximum ids
    putStrLn $ "Max ID: " ++ show ma
    putStrLn $ "Missing seat? " ++ (show $ detectFaults ids)
    where processAll = maximum . (map process)
          process = seatID . guessSeat 




