module Program.Instruction.Parser where

import Program.Instruction
import Program.Mask
import Program.Memory
import Data.Char

chomp :: String -> String
chomp [] = []
chomp (' ':xs) = chomp xs
chomp l = l

tokenize :: String -> [String]
tokenize str =
    let (tks,_:rem) = tokenizeHead str in
        tks ++ [chomp rem]
    where tokenizeHead str =
            let (kw, (x:rem)) = span isAlpha str in
                if x == '[' then
                    let (val, (_:rem')) = break (== ']') rem in
                        ([kw,val],chomp rem')
                else
                    ([kw],chomp rem)

parseOne :: [String] -> Instruction
parseOne ("mask":m:_)  = SetMask $ parseMask m
parseOne ("mem":n:v:_) = SetMem (read n) (read v)

parseLine :: String -> Instruction
parseLine = parseOne . tokenize

parseAll :: String -> [Instruction]
parseAll = (map parseLine) . lines



