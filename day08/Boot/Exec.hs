{-
   Execution engine for the small "boot assembly language" 
   of the puzzle.
   The engine is based on the State monad, and the state is
   a "Registers" type.
   This type records an execution trace (useful for debug).
-}
module Boot.Exec where

import Boot.Inst
import Data.Vector (Vector, (!?))
import Control.Monad.State.Lazy

type InstId = Int

{-
   Engine's state: registers, containing the current program counter,
   a trace of the visited instructions and an accumulator register
   for the Acc instruction.
-}
data Registers = Registers {
        previous_counters :: [InstId],
        program_counter :: InstId,
        accumulator :: Int
    }

{-
   Convenient Show instance for Registers
-}
instance Show Registers where
  show reg =
      "Registers: acc=" ++ show (accumulator reg) ++ ", pc=" ++ show (program_counter reg) ++ "\n - trace: " ++ show (previous_counters reg)

{-
   Main type: a State monad with Registers as its state
-}
type ProgState a = State Registers a

{-
   Program status:
   - Running means the program is still running
   - Stopped means the program has stopped for "natural" reasons, i.e.
   has reached its end
   - Looping means the program is detected to be looping
-}
data Status = Running | Stopped | Looping deriving (Show,Eq,Ord,Enum)

{-
   Add a constant to the program counter (and record its former value
   in the trace).
-}
add_pc :: Int -> ProgState ()
add_pc n = state $ \(Registers pre pc acc) -> ((), Registers (pc:pre) (pc + n) acc)

{-
   Increment the program counter.
-}
inc_pc :: ProgState ()
inc_pc = add_pc 1

{-
   Add a number to the accumulator.
-}
put_acc :: Int -> ProgState ()
put_acc n = state $ \(Registers lc pc acc) -> ((), Registers lc pc (n + acc))

{-
   Execute the instruction pointed by the program counter.
-}
execNext :: Insts -> ProgState Status
execNext insts = do
    pc <- program_counter <$> get
    case insts !? pc of
      Nothing -> return Stopped -- No more instruction => program stopped
      Just inst ->
          case inst of
            Nop _ -> inc_pc >> return Running
            Acc n -> do
                inc_pc
                put_acc n
                return Running
            Jmp n -> add_pc n >> return Running

{-
   Determine if the program is looping by checking if the current
   program counter is already in the trace.

   Note that this works well because there is no branching...
   Otherwise it's kind of undecidable :0)
-}
checkStop :: Status -> ProgState Status
checkStop status = do
    st <- get
    if (program_counter st) `elem` (previous_counters st)
        then return Looping
        else return status

{-
   Build a full run of the program that stops whenever
   stopped or detected to be looping.
-}
runUntilStop :: Insts -> ProgState Status
runUntilStop insts = do
    st <- execNext insts
    st' <- checkStop st
    case st' of
      Running -> runUntilStop insts
      _ -> return st'

{-
   Initial state for the executer (accumulator = 0, no trace,
   program counter = 0
-}
initState :: Registers
initState = Registers {
        previous_counters = [],
        program_counter = 0,
        accumulator = 0
    }

{-
   Run the state monad from the initial state
-}
fromStart :: ProgState a -> (a, Registers)
fromStart sta = runState sta initState


