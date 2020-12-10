{-
   Challenge Day 2: check strings ("passwords") against given
   policies.

   The difficulty here is to parse a line correctly... I wrote
   a "brutal" parser, based on the exact format
   > (\d+).(\d+).(.).*   --> Policy $1 $2 $3
   Once the policy is retrieved, checking it is just a matter
   of writing the correct predicates and use iterators to just
   check the whole list
-}
module PwPolicies where

import Text.Read
import Data.Char
import Control.Monad

{-
   A password policy: two numbers (pmin and pmax; note that 
   these names are adequate for the first challenge but not
   the second... oh well) and a character.
-}
data Policy = Policy { pmin :: Int, pmax :: Int, pchar :: Char } deriving Show

{-
   Parse a policy under the form <(\d+).(\d+).(.).*>
   This parsing is somewhat brutal and absolutely not resilient,
   but it's fine for this kind of challenge. If the input deviates
   even a tiny bit from the pattern, it will return Nothing.
   
   Note the use of Maybe as an applicative functor, which allows
   us to use <$> and <*>, calling the Policy constructor on
   the arguments in a straightforward way while propagating
   Nothing if needed.
-}
parsePolicy :: String -> Maybe Policy
parsePolicy str =
    Policy <$> (readMaybe min) <*> (readMaybe max) <*> (pure ch)
    where (min,_:rem ) = span isNumber str
          (max,_:ch:_) = span isNumber rem

{-
   Parse a "challenge", i.e. a policy with an associated
   password.
   The expected format is
   > <policy>:\s*<pw>
   
   Note the use again of Maybe as an applicative functor,
   and the tuple constructor operator (,).
-}
parseChallenge :: String -> Maybe (Policy, String)
parseChallenge str =
    (,) <$> (parsePolicy pol) <*> (chomp rem)
    where (pol,rem) = span (/= ':') str
          -- chomp is probably unnecessary but just in case...
          chomp [] = Nothing
          chomp (_:xs) = Just $ dropWhile isSpace xs
          
{-
   Small function that count the number of occurrences of a in
   list l
-}
count :: Eq a => a -> [a] -> Int
count a l =
    foldl (\n a' -> if a == a' then n + 1 else n) 0 l

{-
   First policy: checks if the number of occurrences of the given
   characters is between the proposed bounds.
-}
policyOk :: Policy -> String -> Bool
policyOk pol str =
    min <= ct && ct <= max
    where min = pmin pol
          max = pmax pol
          ct = count (pchar pol) str

{-
   Second policy: checks if the given character is at one of the
   proposed index (but not both).
   Note that policies are given starting from 1 (not 0) hence the
   -1.
   `check` is simply a xor.
-}
policyOk' :: Policy -> String -> Bool
policyOk' pol str =
    check (str !! first == ch) (str !! second == ch)
    where first  = pmin pol - 1
          second = pmax pol - 1
          ch     = pchar pol
          check a b = a /= b

{-
   Given a policy check function, parse a list of challenges
   and count the number of lines that are valid.
-}
process :: (Policy -> String -> Bool) -> [String] -> Int
process policy str =
    count True $ map process1 str
    where process1 :: String -> Bool
          process1 str =
              case parseChallenge str of
                Nothing -> False
                Just (pol, str) -> policy pol str



