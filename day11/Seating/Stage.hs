module Seating.Stage where

import qualified Data.Vector as V
import Seating.Seat

data Stage a = Stage {
        nrows :: Int,
        ncols :: Int,
        def :: a,
        content :: V.Vector a
    }

instance Eq a => Eq (Stage a) where
  st == st' = (content st) == (content st')

instance Show a => Show (Stage a) where
  show st =
      foldr (++) "" $ map showL $ group $ V.toList $ content st
      where group [] = []
            group xs =
                let (pre,suf) = splitAt nc xs in
                    pre:(group suf)
            nc = ncols st
            showL l = foldr (++) "\n" $ map show l

at :: Stage a -> (Int,Int) -> a
at st (i,j)
    | i < 0 || j < 0 || i >= (nrows st) || j >= (ncols st) = def st
    | otherwise = V.unsafeIndex (content st) (i*(ncols st) + j)

neighbors :: Stage a -> (Int,Int) -> [a]
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

makeStage :: [[Char]] -> Stage Seat
makeStage l@(x:_) =
    Stage { nrows = nr, ncols = nc, content = ct, def = Floor }
    where nc = length x
          nr = length l
          ct = V.fromList $ map parse $ foldl (++) [] l



