{-
   Challenge Day 1: find a pair (and a triplet) in a list of
   number such that their sum has a given target.

   I could have used Haskell's list monad (I'll use it in a
   later challenge) but I coded a rather "explicit" solution.
-}
module SumProd where


{-
   findPair l t: find a pair (x,y) of elements of list l such
   that x + y = t.
   Returns Just the result if such a pair exists, or Nothing
   else.

   The algorithm takes advantage of a total order on elements
   of type a, and presumes the input list is *sorted* as to
   halt as soon as a pair is found.
-}
findPair :: (Ord a, Eq a, Num a) => [a] -> a -> Maybe (a,a)
findPair [] _ = Nothing
findPair (x:xs) target =
    findOther xs
    where diff = (target - x) -- We have on element of the pair (x); the other has to be (target - x)
          findOther [] = Nothing
          findOther (y:ys)
              | y < diff = findOther ys
              | y == diff = Just (x,y)
              | otherwise = findPair xs target

{-
   findTriple l t: find a triplet (x,y,z) of elements of list l
   such that x + y + z = t.
   Returns Just the result if such a triblet exists, or Nothing
   else.

   This function is based on findPair: if I have the target t and
   a number x, finding the other two number is done with
   > findPair (tail l) (t - x)

   Like for findPair, the function assumes the list is sorted.
-}
findTriplet :: (Ord a, Eq a, Num a) => [a] -> a -> Maybe (a,a,a)
findTriplet [] _ = Nothing
findTriplet (x:xs) target =
    case findPair xs diff of
      Nothing -> findTriplet xs target
      Just (y,z) -> Just (x,y,z)
    where diff = target - x




