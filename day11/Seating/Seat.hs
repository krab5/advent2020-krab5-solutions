{-
   Defines a seat.
   A seat is either "not a seat" (i.e. the floor), a vacant
   seat or an occupied seat.
-}
module Seating.Seat where

data Seat = Floor | Vacant | Occupied deriving (Eq,Ord,Enum)

instance Show Seat where
  show Floor = "."
  show Vacant = "L"
  show Occupied = "#"

{-
   Parse a character as a seat.
   /!\ Unsafe
-}
parse :: Char -> Seat
parse '.' = Floor
parse '#' = Occupied
parse 'L' = Vacant

{-
   Determine if the seat is free.
   Free means either vacant or the floor.
-}
isFree :: Seat -> Bool
isFree Occupied = False
isFree _        = True



