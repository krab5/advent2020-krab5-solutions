{-
   Utility class to handle coordinated, represented as simple
   integer pairs.
-}
module Boat.Coord where

{-
   Coordinates are pairs of integers.
-}
type Coord = (Int,Int)

{-
   Calculate the Manhattan distance
-}
manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) =
    abs (x2 - x1) + abs (y2 - y1)

{-
   Addition on coordinates (simply sum each components
   together)
-}
infixl 6 @+

(@+) :: Coord -> Coord -> Coord
(x1,y1) @+ (x2,y2) = (x1 + x2, y1 + y2)

{-
   Perform the clockwise rotation by 90 degrees of the given
   point around the frame's origin.
-}
rotate90CW :: Coord -> Coord
rotate90CW (x,y) = (y, -x)

{-
   Perform n clockwise rotations by 90 degrees of the given
   point around the frame's origin.
-}
rotate90CWn :: Int -> Coord -> Coord
rotate90CWn 0 c = c
rotate90CWn n c = rotate90CWn (n - 1) $ rotate90CW c

{-
   Perform the clockwise rotation if the given point around
   the frame's origin, by the given angle (must be a multiple
   of 90 as we are on a discrete frame)
-}
rotateCW :: Int -> Coord -> Coord
rotateCW angle = rotate90CWn ((angle `div` 90) `mod` 4)

{-
   Perform the counter-clockwise rotation by 90 degrees of the
   given point around the frame's origin.
-}
rotate90CCW :: Coord -> Coord
rotate90CCW (x,y) = (-y, x)

{-
   Perform n counter-clockwise rotations by 90 degrees of the
   given point around the frame's origin.
-}
rotate90CCWn :: Int -> Coord -> Coord
rotate90CCWn 0 c = c
rotate90CCWn n c = rotate90CCWn (n - 1) $ rotate90CCW c

{-
   Perform the counter-clockwise rotation if the given point
   around the frame's origin, by the given angle (must be a
   multiple of 90 as we are on a discrete frame)
-}
rotateCCW :: Int -> Coord -> Coord
rotateCCW angle = rotate90CCWn ((angle `div` 90) `mod` 4)

{-
   Generate the translation vector that goes north by n unit
-}
north :: Int -> Coord
north n = (0,n)

{-
   Generate the translation vector that goes south by n unit
-}
south :: Int -> Coord
south n = (0,-n)

{-
   Generate the translation vector that goes east by n unit
-}
east :: Int -> Coord
east n = (n,0)

{-
   Generate the translation vector that goes west by n unit
-}
west :: Int -> Coord
west n = (-n,0)


