module Main where

import Seating.Seat
import Seating.Stage
import Seating.Rule
import qualified Data.Vector as V

count :: Foldable t => (a -> Bool) -> t a -> Int
count p =
    foldl (\acc x -> if p x then acc + 1 else acc) 0


rule :: Rule
rule Floor    _ = Nothing
rule Vacant   l
    | all isFree l = Just Occupied
    | otherwise = Nothing
rule Occupied l
    | count (not . isFree) l >= 4 = Just Vacant
    | otherwise = Nothing

file = "seats.txt"

main :: IO ()
main = do
    st <- makeStage <$> lines <$> readFile file
    let (n,st') = updateTilFix rule st in do
        putStrLn $ "Convergence met in " ++ show n ++ " steps."
        putStrLn $ "Number of seats occupied: " ++ (show $ count (== Occupied) (content st'))
        putStrLn "Result:"
        putStrLn $ show st'





