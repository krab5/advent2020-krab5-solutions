module Program.Mask where

import Data.Bits

data Action = X | Set0 | Set1 deriving (Eq,Ord,Enum)
type Mask = [(Int,Action)]

nomask :: Mask
nomask = []

setAll :: Int -> Mask
setAll n = zip [0..(n-1)] $ repeat Set1

clearAll :: Int -> Mask
clearAll n = zip [0..(n-1)] $ repeat Set0

instance Show Action where
  show X = "X"
  show Set0 = "0"
  show Set1 = "1"

printMask :: Int -> Mask -> String
printMask n m =
    map show1 $ reverse [0..(n-1)]
    where show1 n =
              case lookup n m of
                Just Set0 -> '0'
                Just Set1 -> '1'
                _ ->'X'

parseAction :: Char -> Action
parseAction 'X' = X
parseAction '0' = Set0
parseAction '1' = Set1

parseMask :: String -> Mask
parseMask =
    (filter ((/= X) . snd)) . (zip [0..]) . (map parseAction) . reverse

applyAction :: Bits b => b -> (Int,Action) -> b
applyAction b (_,X   ) = b
applyAction b (n,Set0) = clearBit b n
applyAction b (n,Set1) = setBit b n

applyMask :: Bits b => Mask -> b -> b
applyMask m b =
    foldl applyAction b m


