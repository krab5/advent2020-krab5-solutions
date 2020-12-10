{-
   Challenge Day 6: check a bunch of answers to a custom
   form...

   Using sets this is beyond trivial...
-}
module Custom where

import qualified Data.Set as S


{-
   Blockify a list of list (see day 4)
-}
blocks :: [[a]] -> [[[a]]]
blocks =
    (foldr group [[]])
    where group [] acc@([]:_) = acc
          group [] acc        = []:acc
          group l  (x:acc)    = (l:x):acc

{-
   Take a bunch of answers and turn it into a set.
   There are two policies: either get the union of 
   every answers (commented), or get the intersection
   of every answers.
-}
aggregate :: [[Char]] -> S.Set Char
aggregate b =
    foldl1 S.intersection $ map S.fromList b
    --foldl S.union S.empty $ map S.fromList b

{-
   Sum the size of every set.
-}
countAll :: [S.Set Char] -> Int
countAll = foldl (\acc s -> acc + S.size s) 0




