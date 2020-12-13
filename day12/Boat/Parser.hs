{-
   Parser for the input file.
-}
module Boat.Parser where

import Text.Read
import Boat.Instruction

{-
   Parse the letter of each "opcode"
-}
parseLetter :: Char -> Maybe (Int -> Instruction)
parseLetter 'N' = Just $ Go North
parseLetter 'E' = Just $ Go East
parseLetter 'S' = Just $ Go South
parseLetter 'W' = Just $ Go West
parseLetter 'L' = Just $ Turn DLeft
parseLetter 'R' = Just $ Turn DRight
parseLetter 'F' = Just $ Forward
parseLetter _   = Nothing

{-
   Parse a string into an instruction (or Nothing if this
   isn't possible)
-}
parse :: String -> Maybe Instruction
parse [] = Nothing
parse (x:xs) =
    parseLetter x <*> readMaybe xs

{-
   Parse a list of strings into a list of instructions.
-}
parseLines :: [String] -> [Instruction]
parseLines =
    foldr fun []
    where fun x acc =
            case parse x of
              Nothing -> acc
              Just px -> px:acc

{-
   Parse a whole newline separated file into a list of
   instructions
-}
parseAll :: String -> [Instruction]
parseAll = parseLines . lines


