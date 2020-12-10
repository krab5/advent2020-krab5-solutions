module Main where

import PwPolicies
import Control.Monad


file = "passwords.txt"

main :: IO ()
main = do
    ls <- lines <$> readFile file
    putStrLn $ "Valid password found for policy 1: " ++ (show $ process policyOk  ls)
    putStrLn $ "Valid password found for policy 2: " ++ (show $ process policyOk' ls)




