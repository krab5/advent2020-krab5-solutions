module Main where

import Program.Instruction.Parser
import Program.Instruction
import Program.Exec
import Program.State
import Program.Memory

file = "program.txt"

main :: IO ()
main = do
    insts <- parseAll <$> readFile file
    reg <- return $ runFromInit insts
    putStrLn $ "Memory sum: " ++ (show $ sumAll $ mem reg)




