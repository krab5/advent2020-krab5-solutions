{-
   The state for the program execution
-}
module Program.State where

import Program.Memory
import Program.Mask
import Data.Word
import qualified Control.Monad.State as S

{-
   Program basic state: the current mask + the memory
-}
data Registers = Registers {
        mask :: Mask,
        mem  :: Memory
    }

{-
   Convenient Show instance for registers
-}
instance Show Registers where
  show reg = "Current mask: " ++ (printMask 36 $ mask reg) ++ "\nMemory content:\n" ++ (show $ mem reg)

-- State monad
type PState = S.State Registers

{-
   Base registers: no mask and empty memory
-}
initReg :: Registers
initReg = Registers { mask = nomask, mem = emptyMem }

{-
   Set the current mask
-}
setMask :: Mask -> PState ()
setMask m =
    S.state $ \reg -> ((), reg { mask = m })

{-
   Get the current mask
-}
getMask :: PState Mask
getMask =
    S.state $ \reg -> (mask reg, reg)

{-
   Set a value in memory (v1)
-}
setMem :: Addr -> Word64 -> PState ()
setMem addr val = do
    reg <- S.get
    let mem' = set (mask reg) (mem reg) addr val in
        S.put $ reg { mem = mem' }

{-
   Set a value in memory (v2)
-}
setMem' :: Addr -> Word64 -> PState ()
setMem' addr val = do
    reg <- S.get
    let mem' = setWithMask (mask reg) (mem reg) addr val in
        S.put $ reg { mem = mem' }

{-
   Get a value in memory
-}
getMem :: Addr -> PState Word64
getMem addr = do
    reg <- S.get
    return $ get (mem reg) addr

{-
   Sum every value in memory
-}
getMemSum :: PState Word64
getMemSum = do
    reg <- S.get
    return $ sumAll $ mem reg


