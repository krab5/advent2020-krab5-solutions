{-
   A program instruction.
-}
module Program.Instruction where

import Data.Word
import Program.Mask
import Program.Memory

{-
   Instructions are either:
   - set the current mask
   - write a value in memory at the given address
-}
data Instruction =
      SetMask Mask
    | SetMem  Addr Word64 

{-
   Convenient Show instance for instructions
-}
instance Show Instruction where
  show (SetMask m) = "mask = " ++ printMask 36 m
  show (SetMem a v) = "mem[" ++ show a ++ "] = " ++ show v




