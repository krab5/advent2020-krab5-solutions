module Seating.Seat where

data Seat = Floor | Vacant | Occupied deriving (Eq,Ord,Enum)

instance Show Seat where
  show Floor = "."
  show Vacant = "L"
  show Occupied = "#"

parse :: Char -> Seat
parse '.' = Floor
parse '#' = Occupied
parse 'L' = Vacant

isFree :: Seat -> Bool
isFree Occupied = False
isFree _        = True



