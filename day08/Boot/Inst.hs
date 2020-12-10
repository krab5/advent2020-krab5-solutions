{-
   Specification of the instruction language.
-}
module Boot.Inst where

import qualified Data.Vector as V

{-
   Three types of instruction (accumulate, jump, nop)
   Nop has an argument because it is needed in the second part...
   Otherwise it shouldn't!
-}
data Inst = Acc Int | Jmp Int | Nop Int deriving (Eq)

-- For pretty printing purposes:
explicit_sign :: (Ord a, Num a, Show a) => a -> String
explicit_sign n
    | n >= 0 = '+':(show n)
    | otherwise = show n

instance Show Inst where
  show (Acc n) =
      "acc " ++ explicit_sign n
  show (Jmp n) =
      "jmp " ++ explicit_sign n
  show (Nop n) =
      "nop " ++ explicit_sign n

type Insts = V.Vector Inst



