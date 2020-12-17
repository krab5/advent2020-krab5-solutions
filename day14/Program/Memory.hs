{-
   A memory model for the program
   Because memory was getting cramped, I had to model it
   using binary trees...
-}
module Program.Memory where

import Program.Mask
import Data.Bits
import Data.Word
import Text.Printf

{-
   Basic data structure for the memory: left trees correspond
   to 0 bit, right trees correspond to 1 bit.
   For example, an address such as "0101" means that we look
   at value at left, then right, then left, then right (MSB 
   first)
-}
data MTree = MLeaf Word64 | MNode MTree MTree

{-
   Empty tree
-}
emptyTree :: MTree
emptyTree = MLeaf 0

{-
   Get a value in the tree for a given address, considering
   the n LSB of addr.
-}
getInTree :: Bits b => MTree -> Int -> b -> Word64
getInTree (MLeaf v)   _ addr = v
getInTree (MNode l r) n addr =
    getInTree subtree (n - 1) addr
    where subtree = if testBit addr (n - 1) then r else l

{-
   Set the value in the tree for the given address, considering
   the n LSB of addr
-}
updateInTree :: Bits b => MTree -> Int -> b -> Word64 -> MTree
updateInTree (MLeaf _) 0 _ w = MLeaf w
updateInTree (MLeaf _) n addr w =
    if testBit addr (n - 1)
        then MNode (MLeaf 0) (updateInTree (MLeaf 0) (n - 1) addr w)
        else MNode (updateInTree (MLeaf 0) (n - 1) addr w) (MLeaf 0)
updateInTree (MNode l r) n addr w =
    if testBit addr (n - 1)
        then MNode l (updateInTree r (n - 1) addr w)
        else MNode (updateInTree l (n - 1) addr w) r

type Addr = Word64

{-
   Get all non-null value in the tree, with their associated address
-}
getAllNon0 :: Int -> MTree -> [(Addr,Word64)]
getAllNon0 _ (MLeaf 0) = []
getAllNon0 _ (MLeaf v) = [(0, v)]
getAllNon0 n (MNode l r) =
    ll ++ rl
    where ll = getAllNon0 (n - 1) l
          rl0= getAllNon0 (n - 1) r
          rl = map (\(a,v) -> (setBit a (n - 1),v)) rl0

{-
   The memory: a tree + a set bit size
-}
data Memory = Memory {
        bitsize :: Int,
        content :: MTree
    }

{-
   Convenient Show instance for Memory
-}
instance Show Memory where
  show mem =
      foldl (++) "" $ map show1 ct
      where ct = getAllNon0 (bitsize mem) (content mem)
            show1 (ad,w) = printf "- [% 7d] %036b (decimal %d)\n" ad w w

{-
   Empty memory (on 36 bits)
-}
emptyMem :: Memory
emptyMem = Memory 36 emptyTree

{-
   Size of the memory (i.e. number of non-null values)
-}
memSize :: Memory -> Int
memSize mem = length $ getAllNon0 (bitsize mem) (content mem)

{-
   Get a value in the memory at the given address
-}
get :: Memory -> Addr -> Word64
get mem addr = getInTree (content mem) (bitsize mem) addr

{-
   Set the value in the memory at the given address after
   applying the mask
-}
set :: Mask -> Memory -> Addr -> Word64 -> Memory
set mask mem addr val =
    mem { content = t' }
    where t' = updateInTree (content mem) (bitsize mem) addr val'
          val' = applyMask mask val  

{-
   Sum every value in the memory
-}
sumAll :: Memory -> Word64
sumAll mem =
    sumTree $ content mem
    where sumTree (MLeaf v) = v
          sumTree (MNode l r) = sumTree l + sumTree r

{-
   Set the value in the memory, using the mask to determine
   the addresses.
-}
setWithMask :: Mask -> Memory -> Addr -> Word64 -> Memory
setWithMask mask mem addr val =
    mem { content = t' }
    where t' = foldl updateOne (content mem) addrs
          updateOne amem add = updateInTree amem (bitsize mem) add val
          addrs = applyMask' mask addr






