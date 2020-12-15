module Program.Exec where

import Program.State
import Program.Instruction
import Control.Monad.State

execInst :: Instruction -> PState ()
execInst (SetMask m) = setMask m
execInst (SetMem a v) = setMem a v

execAll :: [Instruction] -> PState ()
execAll [] = return ()
execAll (x:xs) = (execInst x) >> (execAll xs)

runFromInit :: [Instruction] -> Registers
runFromInit insts = snd $ runState (execAll insts) initReg



