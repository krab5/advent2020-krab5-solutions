{-
   Model a "stage", i.e. a grid of seats.
-}
module Seating.Stage where

import qualified Data.Vector as V
import Seating.Seat

{-
   A stage is modelled as a vector of seats, with
   a number of rows and a number of columns.
   The vector is linear, and for a given index (i,j)
   the value of stage at that index is the value of
   the vector at index (i * <num columns> + j).
   (this is a classical one-dimension "by-column" matrix)
-}
data Stage = Stage {
        nrows :: Int,
        ncols :: Int,
        content :: V.Vector Seat
    }

{-
   Eq typeclass instance for Stage (used during fixpoint
   calculus).
-}
instance Eq Stage where
  st == st' = (content st) == (content st')

{- 
    Convenient Show instance for Stage.
    Ideally, show $ makeStage $ lines str == str
-}
instance Show Stage where
  show st =
      foldr (++) "" $ map showL $ group $ V.toList $ content st
      where group [] = []
            group xs =
                let (pre,suf) = splitAt nc xs in
                    pre:(group suf)
            nc = ncols st
            showL l = foldr (++) "\n" $ map show l

{-
   Determines if the given position is in bounds of the
   given stage.
-}
inBounds :: (Int,Int) -> Stage -> Bool
inBounds (i,j) st =
    i >= 0 && j >= 0 && i < nr && j < nc
    where nr = nrows st
          nc = ncols st

{-
   Give the seat of the stage at the given position,
   or Nothing if the position is out of bounds.
-}
atM :: Stage -> (Int,Int) -> Maybe Seat
atM st (i,j)
    | (i,j) `inBounds` st = Just $ V.unsafeIndex (content st) (i*(ncols st) + j)
    | otherwise = Nothing

{-
   Give the seat of the stage at the given position,
   or Floor if the position is out of bounds.
-}
at :: Stage -> (Int,Int) -> Seat
at st pos =
    case st `atM` post of
      Nothing -> Floor
      Just x -> x

{-
   Sum two pairs of numbers.
-}
tsum :: (Int,Int) -> (Int,Int) -> (Int,Int)
tsum (x,y) (i,j) = (x + i, y + j)

{-
   "Look" in a direction in a stage, from a starting
   position, and return the first non-floor seat observed
   in that direction, or Floor if there isn't.
-}
lookAt :: Stage -> (Int,Int) -> (Int,Int) -> Seat
lookAt st pos dir =
    case st `atM` pos' of
      Nothing -> Floor
      Just Floor -> lookAt st pos' dir
      Just x -> x
    where pos' = pos `tsum` dir

{-
   Calculate the neighbors of the given position in the
   stage.
-}
neighbors :: Stage -> (Int,Int) -> [Seat]
neighbors st (i,j) =
    map (at st) indices
    where indices = [ (i-1,j-1)      -- Top Left
                    , (i-1,j  )      -- Top
                    , (i-1,j+1)      -- Top Right
                    , (i  ,j-1)      -- Left
                    , (i  ,j+1)      -- Right
                    , (i+1,j-1)      -- Bottom Left
                    , (i+1,j  )      -- Bottom
                    , (i+1,j+1) ]    -- Bottom Right

{-
   Calculate the "far" neighbors of the given position
   in the stage. Far neighbors are first non-floor seats
   in each direction (or Floor if there isn't such a seat).
-}
farNeighbors :: Stage -> (Int,Int) -> [Seat]
farNeighbors st pos =
    map (lookAt st pos) indices
    where indices = [ (-1,-1)      -- Top Left
                    , (-1, 0)      -- Top
                    , (-1, 1)      -- Top Right
                    , ( 0,-1)      -- Left
                    , ( 0, 1)      -- Right
                    , ( 1,-1)      -- Bottom Left
                    , ( 1, 0)      -- Bottom
                    , ( 1, 1) ]    -- Bottom Right

{-
   Parse a list of strings as a stage.
-}
makeStage :: [[Char]] -> Stage
makeStage l@(x:_) =
    Stage { nrows = nr, ncols = nc, content = ct, def = Floor }
    where nc = length x
          nr = length l
          ct = V.fromList $ map parse $ foldl (++) [] l



