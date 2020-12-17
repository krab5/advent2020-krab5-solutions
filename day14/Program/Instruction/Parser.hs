{-
   Parser for the program.
   For this language parsing is a bit cumbersome...
   I decided to put it in a separate module.
   The parser is not really reliable but it works for the
   exercise.
-}
module Program.Instruction.Parser where

import Program.Instruction
import Program.Mask
import Program.Memory
import Data.Char

{-
   Removes leading spaces
-}
chomp :: String -> String
chomp [] = []
chomp (' ':xs) = chomp xs
chomp l = l

{-
   Transform a string into a list of tokens.
   The interesting tokens are:
    - keyword
    - memory address
    - value (mask or number)
-}
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

{-
   A (very) rudiemntary grammar:
   Mask -> "mask" "=" <mask>
   Mem  -> "mem" "[" <addr> "]" "=" <value>
-}
parseOne :: [String] -> Instruction
parseOne ("mask":m:_)  = SetMask $ parseMask m
parseOne ("mem":n:v:_) = SetMem (read n) (read v)

{-
   Parse one line
-}
parseLine :: String -> Instruction
parseLine = parseOne . tokenize

{-
   Parse a string (with several lines)
-}
parseAll :: String -> [Instruction]
parseAll = (map parseLine) . lines



