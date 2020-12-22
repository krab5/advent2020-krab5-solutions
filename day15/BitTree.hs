{-
   A Bit binary tree: allows to store value at a given
   address, expressed as a bit array.
   This is relatively efficient, while taking advantage of
   sparse number distribution.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module BitTree where

import Data.List (sortBy)
import Data.Bits
import Data.Word
import Text.Printf

{-
   A bit tree: left trees correspond to 0, right trees to 1,
   starting with the least significant bit.
-}
data BitTree a = Leaf a | Node (BitTree a) (BitTree a) 

{-
   An empty tree is a single leaf with a "default value"
-}
emptyTree :: a -> BitTree a
emptyTree = Leaf

{-
   Get the value at the given address (bit array) or a default
   value if none has been found.
-}
get :: Bits b => a -> BitTree a -> b -> a
get def (Leaf v) addr
    | addr == zeroBits = v
    | otherwise = def
get def (Node l r) addr =
    get def subtree (shiftR addr 1)
    where subtree = if testBit addr 0 then r else l

{-
   Update the bit tree with the new value, creating default leafs
   at the required places.
-}
update :: Bits b => a -> BitTree a -> b -> a -> BitTree a
update def (Leaf ro) addr val
    | addr == zeroBits = Leaf val
    | otherwise =
        if testBit addr 0
            then Node (Leaf ro) (update def (Leaf def) (shiftR addr 1) val)
            else Node (update def (Leaf ro) (shiftR addr 1) val) (Leaf def)
update def (Node l r) addr val =
    if testBit addr 0
        then Node l (update def r (shiftR addr 1) val)
        else Node (update def l (shiftR addr 1) val) r

{-
   Update a batch of values in the tree
-}
updateAll :: Bits b => a -> BitTree a -> [(b,a)] -> BitTree a
updateAll def t l =
    foldl (\tacc (addr,val) -> update def tacc addr val) t l

{-
   Make a bit tree containing integers from the given list of
   addresses.
-}
makeBTree :: Bits b => [b] -> BitTree Int
makeBTree l = updateAll 0 (emptyTree 0) $ zip l [1..]

{-
   Get any (k,v) pairs contained in the bit tree
-}
getAll :: Bits b => BitTree a -> [(b,a)]
getAll (Leaf v) = [(zeroBits,v)]
getAll (Node l r) =
    let ll = map shiftL1 $ getAll l
        lr = map shiftL1 $ getAll r in
        ll ++ (map setBit0 lr)
    where shiftL1 (x,v) = (shiftL x 1,v)
          setBit0 (x,v) = (setBit x 0,v)

{-
   Print a bit tree, excluding default values
-}
printTree :: forall a. (Eq a, Show a) => a -> BitTree a -> String
printTree def t =
    show l
    where l = sortBy cmp $ filter ((/= def) . snd) $ pairs
          pairs :: [(Word64,a)]
          pairs = getAll t
          cmp (a,_) (b,_) = compare a b

{-
   Convenient show instance for bit trees
-}
instance (Num a, Eq a, Show a) => Show (BitTree a) where
  show = printTree 0

{-
   Search the given value as address in the tree.
   If it exists, return the difference between the stored value
   and the given id; if it does not, return 0.
   Update the tree, associating id with the address.
-}
searchAndUpdate :: Int -> Int -> BitTree Int -> (Int,BitTree Int)
searchAndUpdate val id tree =
    (res,tree')
    where n = get 0 tree val
          res = if n == 0 then 0 else id - n
          tree' = update 0 tree val id




