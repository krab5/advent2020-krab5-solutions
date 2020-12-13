{-
   Rule system for the cellular automaton.
   We have two concepts: a "neighbor" function that, given
   a stage and a seat, determines the neighboring seat,
   and a "rule" that, given a list of neighbors and a seat,
   return the next state of that seat.
-}
module Seating.Rule where

import qualified Data.Vector as V
import Seating.Seat
import Seating.Stage

{-
   A neighbor calculation function
-}
type Neighborhood = Stage -> (Int,Int) -> [Seat]

{-
   A rule
-}
type Rule = Seat -> [Seat] -> Maybe Seat

{-
   Given a neighbor function, a rule, a stage and a position,
   calculate the next state of the seat, if it changes (or Nothing
   if it doesn't).
-}
nextSeat :: Neighborhood -> Rule -> Stage -> (Int,Int) -> Maybe Seat
nextSeat neighbors rule st (i,j) =
    val'
    where index = i*(ncols st) + j
          val  = V.unsafeIndex (content st) index
          val' = rule val (neighbors st (i,j))

{-
   Given a neighbor function, a rule and a stage, calculate the
   next state of the stage.
-}
updateStage :: Neighborhood -> Rule -> Stage -> Stage 
updateStage neighbors rule st =
    st { content = content' }
    where nc = ncols st
          nr = nrows st
          allids = [ (i,j) | j <- enumFromTo 0 (nc - 1)
                           , i <- enumFromTo 0 (nr - 1) ]
          fun x@(i,j) acc =
              case nextSeat neighbors rule st x of
                Nothing -> acc
                Just s -> (i*nc+j,s):acc
          ups = V.fromList $ foldr fun [] allids
          content' = V.update (content st) ups

{-
   Update the stage until it does not progress anymore.
   Careful, there's no proof this terminates :S
-}
updateTilFix :: Neighborhood -> Rule -> Stage -> (Int, Stage)
updateTilFix neighbors rule init =
    stop $ iterate update (0,init)
    where update (n,st) = (n+1, updateStage neighbors rule st)
          stop (x1:x2:xs)
              | snd x1 == snd x2 = x1
              | otherwise = stop (x2:xs)



