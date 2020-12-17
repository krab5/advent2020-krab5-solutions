{-
   A mean to execute program, based on the state monad.
   The use of the state monad is not mandatory of course,
   but it is quite convenient.
-}
module Program.Exec where

import Program.State
import Program.Instruction
import Control.Monad.State

{-
   Exec instructions (v1)
-}
execInst :: Instruction -> PState ()
execInst (SetMask m) = setMask m
execInst (SetMem a v) = setMem a v

{-
   Exec instructions (v2)
-}
execInst' :: Instruction -> PState ()
execInst' (SetMask m) = setMask m
execInst' (SetMem a v) = setMem' a v

{-
   Exec a list of instructions with the given specific
   function.
-}
execAll0 :: (Instruction -> PState ()) -> [Instruction] -> PState ()
execAll0 f [] = return ()
execAll0 f (x:xs) = (f x) >> (execAll0 f xs)

execAll  = execAll0 execInst
execAll' = execAll0 execInst'

{-
   Run the full program from an empty memory with a nil
   mask (v1)
-}
runFromInit :: [Instruction] -> Registers
runFromInit insts = snd $ runState (execAll insts) initReg

{-
   Run the full program from an empty memory with a nil
   mask (v2)
-}
runFromInit' :: [Instruction] -> Registers
runFromInit' insts = snd $ runState (execAll' insts) initReg



