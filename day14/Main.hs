{-
   Challenge day 14: program emulation \o/
-}
module Main where

import Program.Instruction.Parser
import Program.Instruction
import Program.Exec
import Program.State
import Program.Mask
import Program.Memory

file = "program.txt"

main :: IO ()
main = do
    putStrLn "Parsing instructions."
    insts <- parseAll <$> readFile file
    putStrLn "Run version 1" -- Mask to value
    reg <- return $ runFromInit insts
    putStrLn $ "Memory size: " ++ (show $ memSize $ mem reg)
    putStrLn $ "Memory sum: " ++ (show $ sumAll $ mem reg)
    putStrLn "Run version 2" -- Mask to address
    reg' <- return $ runFromInit' insts
    putStrLn $ "Memory size: " ++ (show $ memSize $ mem reg')
    putStrLn $ "Memory sum: " ++ (show $ sumAll $ mem reg')




