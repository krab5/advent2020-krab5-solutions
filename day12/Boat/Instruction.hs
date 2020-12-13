{-
   Model the possible instructions for the boat, as found in the file.
-}
module Boat.Instruction where

-- A Cardinal point
data Cardinal = North | East | South | West deriving (Eq,Ord,Enum)
-- A rotation direction
data Direction = DLeft | DRight deriving (Eq,Ord,Enum)
-- An instruction
data Instruction =
      Go Cardinal Int
    | Turn Direction Int
    | Forward Int
    deriving (Eq)

-- Convenient show instance for each type
instance Show Cardinal where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

instance Show Direction where
  show DLeft  = "L"
  show DRight = "R"

instance Show Instruction where
  show (Go c n) = show c ++ show n
  show (Turn d n) = show d ++ show n
  show (Forward n) = "F" ++ show n

{-
   Calculate the cardinal you face when you rotate 90
   degrees clockwise.
-}
nextRight :: Cardinal -> Cardinal
nextRight West = North
nextRight x = succ x

{-
   Calculate the cardinal you face when you rotate 90
   degrees counter-clockwise.
-}
nextLeft :: Cardinal -> Cardinal
nextLeft North = West
nextLeft x = pred x

{-
   Calculate the cardinal you face when you rotate 90
   degrees in the given direction.
-}
nextDir :: Direction -> Cardinal -> Cardinal
nextDir DRight = nextRight
nextDir DLeft  = nextLeft

{-
   Calculate the cardinal you face when you rotate by the given
   angle in the given direction.
-}
rotate :: Direction -> Int -> Cardinal -> Cardinal
rotate dir angle =
    rotate1 num
    where num = (angle `div` 90) `mod` 4
          rotate1 0 x = x
          rotate1 n x = rotate1 (n - 1) $ nextDir dir x



