{-
   Define a Mask, which is a succession of symbols.
-}
module Program.Mask where

import Data.Bits

-- A mask symbol: X for unchanged, Set0 for 0 and Set1 for 1
data Action = X | Set0 | Set1 deriving (Eq,Ord,Enum)

-- A mask is a list of (n,a) where n is the bit order and a
-- is an action
type Mask = [(Int,Action)]

-- Empty mask
nomask :: Mask
nomask = []

-- (for testing) generate a mask with only 1s
setAll :: Int -> Mask
setAll n = zip [0..(n-1)] $ repeat Set1

-- (for testing) generate a mask with only 0s
clearAll :: Int -> Mask
clearAll n = zip [0..(n-1)] $ repeat Set0

{-
   Convenient Show instance for actions
-}
instance Show Action where
  show X = "X"
  show Set0 = "0"
  show Set1 = "1"

{-
   Print a mask on the given number of bits.
   This isn't very efficient but who needs efficiency for that?
-}
printMask :: Int -> Mask -> String
printMask n m =
    map show1 $ reverse [0..(n-1)]
    where show1 n =
              case lookup n m of
                Just Set0 -> '0'
                Just Set1 -> '1'
                _ ->'X'

{-
   Parse a symbol (character) into an action.
-}
parseAction :: Char -> Action
parseAction 'X' = X
parseAction '0' = Set0
parseAction '1' = Set1

{-
   Parse a mask, i.e. a string of symbols
-}
parseMask :: String -> Mask
parseMask = (zip [0..]) . (map parseAction) . reverse

{-
   Apply an action v1: X = unchanged, 0 = clear, 1 = set
-}
applyAction :: Bits b => b -> (Int,Action) -> b
applyAction b (_,X   ) = b
applyAction b (n,Set0) = clearBit b n
applyAction b (n,Set1) = setBit b n

{-
   Apply an action v2: X = 1 and 0, 0 = unchanged, 1 = 1
-}
applyAction' :: Bits b => b -> (Int,Action) -> [b]
applyAction' b (n,X   ) = [setBit b n, clearBit b n]
applyAction' b (n,Set0) = [b]
applyAction' b (n,Set1) = [setBit b n]

{-
   Apply a mask (v1)
-}
applyMask :: Bits b => Mask -> b -> b
applyMask m b =
    foldl applyAction b m

{-
   Apply a mask (v2)
   This generates a set of addresses
-}
applyMask' :: Bits b => Mask -> b -> [b]
applyMask' m b =
    foldr applyAct0 [b] m
    where applyAct0 (n,a) acc =
              concat $ map (\x -> applyAction' x (n,a)) acc


