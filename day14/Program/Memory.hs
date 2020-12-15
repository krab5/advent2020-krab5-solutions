module Program.Memory where

import Program.Mask
import Data.Bits
import Data.Word
import Text.Printf

type Addr = Int

data Memory = Memory {
        content :: [(Addr,Word64)]
    }

instance Show Memory where
  show mem =
      foldl (++) "" $ map show1 ct
      where ct = content mem
            show1 (ad,w) = printf "- [% 7d] %036b (decimal %d)\n" ad w w

emptyMem :: Memory
emptyMem = Memory []

get :: Memory -> Addr -> Word64
get mem addr =
    case lookup addr $ content mem of
      Nothing -> 0
      Just x  -> x

set :: Mask -> Memory -> Addr -> Word64 -> Memory
set mask mem addr val =
    mem { content = set0 $ content mem }
    where set0 [] = [(addr,val')]
          set0 l@((xa,xv):xs)
              | addr == xa = (xa,val'):xs
              | xa > addr = (addr,val'):l
              | otherwise = (xa,xv):(set0 xs)
          val' = applyMask mask val

sumAll :: Memory -> Word64
sumAll mem =
    sum $ (map snd) $ content mem


