{-
   A module to generate mutations of a program.
   Two possible mutations:
   > Jmp n => Nop n
   > Nop n => Jmp n
   One mutation is generated at a time
-}
module Boot.Mutator where

import Boot.Inst
import qualified Data.Vector as V

{-
   Alter an instruction if possible.
   We will filter out the mutations that "gives out nothing" as a
   small optimisation.
-}
alter :: Inst -> Maybe Inst
alter (Nop 0) = Nothing
alter (Nop x) = Just $ Jmp x
alter (Jmp x) = Just $ Nop x
alter x       = Nothing

{-
   Generate a list of mutant from a list of instructions.
   Each mutant (num,insts) contains the number of the instruction
   that has been latered as well as the resulting altered program.
-}
mutate :: Insts -> [(Int,Insts)]
mutate insts =
    V.ifoldl next [] insts
    where next acc n x =
            case alter x of
              Nothing -> acc
              Just x' ->
                  let v = V.update insts $ V.singleton (n, x') in
                      (n,v):acc



