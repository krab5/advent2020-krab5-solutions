module Program.Instruction where

import Data.Word
import Program.Mask
import Program.Memory

data Instruction =
      SetMask Mask
    | SetMem  Addr Word64 

instance Show Instruction where
  show (SetMask m) = "mask = " ++ printMask 36 m
  show (SetMem a v) = "mem[" ++ show a ++ "] = " ++ show v




