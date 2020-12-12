module Seating.Rule where

import qualified Data.Vector as V
import Seating.Seat
import Seating.Stage

type Rule = Seat -> [Seat] -> Maybe Seat

nextSeat :: Rule -> Stage Seat -> (Int,Int) -> Maybe Seat
nextSeat rule st (i,j) =
    val'
    where index = i*(ncols st) + j
          val  = V.unsafeIndex (content st) index
          val' = rule val (neighbors st (i,j))

updateStage :: Rule -> Stage Seat -> Stage Seat
updateStage rule st =
    st { content = content' }
    where nc = ncols st
          nr = nrows st
          allids = [ (i,j) | j <- enumFromTo 0 (nc - 1)
                           , i <- enumFromTo 0 (nr - 1) ]
          fun x@(i,j) acc =
              case nextSeat rule st x of
                Nothing -> acc
                Just s -> (i*nc+j,s):acc
          ups = V.fromList $ foldr fun [] allids
          content' = V.update (content st) ups

updateTilFix :: Rule -> Stage Seat -> (Int, Stage Seat)
updateTilFix rule init =
    stop $ iterate update (0,init)
    where update (n,st) = (n+1, updateStage rule st)
          stop (x1:x2:xs)
              | snd x1 == snd x2 = x1
              | otherwise = stop (x2:xs)



