module Program.State where

import Program.Memory
import Program.Mask
import Data.Word
import qualified Control.Monad.State as S

data Registers = Registers {
        mask :: Mask,
        mem  :: Memory
    }

instance Show Registers where
  show reg = "Current mask: " ++ (printMask 36 $ mask reg) ++ "\nMemory content:\n" ++ (show $ mem reg)

type PState = S.State Registers

initReg :: Registers
initReg = Registers { mask = nomask, mem = emptyMem }

setMask :: Mask -> PState ()
setMask m =
    S.state $ \reg -> ((), reg { mask = m })

getMask :: PState Mask
getMask =
    S.state $ \reg -> (mask reg, reg)

setMem :: Addr -> Word64 -> PState ()
setMem addr val = do
    reg <- S.get
    let mem' = set (mask reg) (mem reg) addr val in
        S.put $ reg { mem = mem' }

getMem :: Addr -> PState Word64
getMem addr = do
    reg <- S.get
    return $ get (mem reg) addr

getMemSum :: PState Word64
getMemSum = do
    reg <- S.get
    return $ sumAll $ mem reg


