{-
   A simple (and far from robust) parser for luggage rules.
-}
module Luggage.Parser where

import Data.List
import Luggage.Rules

{-
   Tokenize a list of words, splitting by (\s+).
   There is a "cleaning" process to remove any remaining
   coma and period.

   Note that tokenize is "fold-like"; but we actually advance
   in the string word-by-word.
-}
tokenize :: String -> [String]
tokenize [] = []
tokenize str =
    let (block, rem) = break (== ' ') str in
        (clean block):(tokenize $ chomp rem)
    where chomp (' ':xs) = chomp xs
          chomp xs = xs
          clean = takeWhile (not . flip elem ",.")

{-
   This is the remnant of a former solution: we show colors
   with an underscore instead of a space
-}
mkcolor :: String -> String -> String
mkcolor col1 col2 = col1 ++ ('_':col2)

{-
   Parse a rule "fragments" (see Luggage.Rules).
   A fragment is basically a description of a thing a bag
   must contain.
   Fragments are of the form:
   - "<num> <color> bag(s)"
   - "no other bags"
   The idea is to parse a list of words until the pattern shows up.

   Careful as the color is ALWAYS in two words.
-}
parseFragment :: [String] -> [RuleFragment]
-- Base case
parseFragment [] = []
-- "no other bags" is usually at the end but you'r never too sure...
parseFragment l@("no":"other":"bags":xs) = parseFragment xs
-- "<num> <attribute> <color> (something)" ...
parseFragment l@(num:col1:col2:_:xs) =
    let r = RuleFragment (mkcolor col1 col2) (read num) in
        (r:) $ parseFragment xs
-- For exhaustivity of the pattern matching; this may happen at the
-- end of a line; we simply skip the current word (eventually this 
-- may lead to function termination which is good).
parseFragment (_:xs) = parseFragment xs

{-
   Parse a line of text as a rule.
   A rule is of the form:
   > <color> bags contain [fragments]
   Beware as <color> is in two pieces.
-}
parseLine :: String -> Rule
parseLine str =
    Rule (mkcolor col1 col2) fragments
    where tkns = tokenize str
          (col1:col2:"bags":"contain":xs) = tkns
          fragments = parseFragment xs

{-
   Parse a whole file of rules into a list of rules.
-}
parse :: String -> [Rule]
parse content =
    map parseLine $ lines content



