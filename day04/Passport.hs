{-
   Challenge Day 4: validate a bunch of passports...

   The idea here is to parse blocks of text as password
   and validate the resulting data.

   This is tedious and error-prone, but not very hard...
-}
module Passport where

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad (join)
import Text.Read

{-
   A passport. Field names are taken from the general
   taxonomy used in the input file.
   Note that countr ID (cid) is a Maybe as we want it to be
   optional.
-}
data Passport = Passport {
        byr :: Int,
        iyr :: Int,
        eyr :: Int,
        hgt :: String,
        hcl :: String,
        ecl :: String,
        pid :: String,
        cid :: Maybe Int
    } deriving (Eq, Show)

{-
   "Parse" one passport from a list of (key,value)s.
   Here we use Maybe as a monad to propagate the Nothings.
   The only subtlety is the lookupAndParse that combines a
   lookup and a readMaybe.
-}
parsePassport :: [(String,String)] -> Maybe Passport
parsePassport table = do
    byr <- lookupAndParse "byr" table
    iyr <- lookupAndParse "iyr" table
    eyr <- lookupAndParse "eyr" table
    hgt <- lookup "hgt" table
    hcl <- lookup "hcl" table
    ecl <- lookup "ecl" table
    pid <- lookup "pid" table
    return $ Passport byr iyr eyr hgt hcl ecl pid $ lookupAndParse "cid" table
    where lookupAndParse :: (Eq a, Read b) => a -> [(a,String)] -> Maybe b
          lookupAndParse key table = join $ readMaybe <$> lookup key table

{-
   Validate the data in a passport.
   Very boring. Just a bunch of predicates.
-}
isValid :: Maybe Passport -> Bool
isValid Nothing = False
isValid (Just (Passport byr iyr eyr hgt hcl ecl pid _)) =
    validByr && validIyr && validEyr && validHgt && validHcl hcl && validEcl && validPid
    where validByr = byr >= 1920 && byr <= 2002
          validIyr = iyr >= 2010 && iyr <= 2020
          validEyr = eyr >= 2020 && eyr <= 2030
          validHgt =
              let (num,uni) = span isDigit hgt in
                  validHgt' (readMaybe num) uni
          validHgt' Nothing _ = False
          validHgt' (Just l) "in" = l >= 59 && l <= 76
          validHgt' (Just l) "cm" = l >= 150 && l <= 193
          validHgt' _ _ = False
          validHcl ('#':xs) = (length xs == 6) && (all validDigit xs)
          validHcl _ = False
          validDigit c = c `elem` (['a'..'f'] ++ ['0'..'9'])
          validEcl = ecl `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
          validPid = (length pid == 9) && (all isDigit pid)


-- transform list of lines into blocks
{-
   The only interesting function in this whole module!
   This turns a list of list into a list of _blocks_.
   A block is a list of list, separated from the other blocks
   by empty lists.

   Basically, in the list of list:
   > [[a,b,c],[d,e],[],[],[g,h,i],[],[j,k],[l]]
   We have three blocks:
   > [[a,b,c],[d,e]]
   > [[g,h,i]]
   > [[j,k],[l]]

   This works on any kind of list but is especially useful to
   parse lines of a file!
   > line 1
   > line 2
   >
   > line 4
   >
   > line 5
   > line 6
   Yields 3 blocks: lines 1 & 2, 4 and 5 & 6.
-}
blocks :: [[a]] -> [[[a]]]
blocks =
    (foldr group [[]])
    where group [] acc@([]:_) = acc
          group [] acc        = []:acc
          group l  (x:acc)    = (l:x):acc

{-
   Parse one line as a number of valued fields (or "entries").
   Valued fields are separated by spaces.
-}
lineEntries :: String -> [String]
lineEntries str =
    entry1 $ dropWhile isSpace str
    where entry1 [] = []
          entry1 str =
              let (one,rem) = break isSpace str in
                  one:(entry1 $ dropWhile isSpace rem)

{-
   Get all the entries of a block.
-}
entries :: [String] -> [String]
entries =
    flatten . (map lineEntries)
    where flatten = foldl (++) []

{-
   Parse an entry as a (key,value) pair.
   Entries are expected to be of the form:
   > <key>:<value>
-}
pairs :: [String] -> [(String,String)]
pairs =
    map pair1
    where pair1 = (\(a,b) -> (a, tail b)) . span (/= ':')

{-
   Get a bunch of (key,value) lists from a bunch of
   lines.
-}
parseAll :: [String] -> [[(String,String)]]
parseAll =
    map (pairs . entries) . blocks

{-
   Get a buch of passports (maybe?) from a bunch of lines.
-}
getPassports :: [String] -> [Maybe Passport]
getPassports = (map parsePassport) . parseAll

{-
   Count the number of elements that validates a given a predicate.
-}
count :: (a -> Bool) -> [a] -> Int
count p =
    foldl (\n a -> if p a then n + 1 else n) 0






