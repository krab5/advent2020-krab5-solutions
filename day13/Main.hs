module Main where

import Data.List (sortBy)
import Text.Read (readMaybe)

parseIds :: String -> [Int]
parseIds "" = []
parseIds str =
    case readMaybe num of
      Nothing -> parseIds rem'
      Just n -> n:(parseIds rem')
    where (num,rem) = break (== ',') str
          rem' =
              case rem of
                (',':xs) -> xs
                _ -> rem

parseFile :: String -> (Int,[Int])
parseFile str =
    (timestamp,ids)
    where (sts:sids:_) = lines str
          timestamp = read sts
          ids = parseIds sids

classify :: Int -> [Int] -> [(Int,Int)]
classify timestamp ids =
    sortBy ordt $ zip ids diff
    where diff = map calc ids
          calc x = ((timestamp `div` x) + 1) * x - timestamp
          ordt x y = compare (snd x) (snd y)

euclide :: Int -> Int -> (Int,Int)
euclide a b =
    let (_, u, v) = euclide1 a 1 0 b 0 1 in
        (u, v)
    where euclide1 r u v 0  u' v' = (r, u, v)
          euclide1 r u v r' u' v' =
              let q = div r r' in
                  euclide1 r' u' v' (r - q * r') (u - q * u') (v - q * v')

solvePrime :: Int -> Int -> Int -> (Int,Int)
solvePrime a b n =
    let (u, v) = euclide a b in
        (u * n, v * n)

solve :: Int -> Int -> Int -> Maybe (Int -> (Int,Int))
solve a b n
    | n `mod` (gcd a b) /= 0 = Nothing
    | otherwise =
        let (u, v) = solvePrime a' b' n' in
            Just $ \k -> (u + b * k `div` delta, v - a * k `div` delta)
        where delta = gcd a b
              a' = a `div` delta
              b' = b `div` delta
              n' = n `div` delta

solveAll :: Int -> [(Int,Int)] -> Maybe [(Int -> (Int,Int))]
solveAll a l =
    sequence $ map (uncurry (solve (-a))) l

generateSolutions :: [(Int -> (Int,Int))] -> [[(Int,Int)]]
generateSolutions l =
    map (\f -> map f [0..]) l

generateTimestamps :: Int -> [[(Int,Int)]] -> [[Int]]
generateTimestamps p l =
    map (map $ ((*) p) . fst) l

inter :: [Int] -> [Int] -> [Int]
inter s (x:xs) = dropWhile (< x) s

file = "buses.txt"

main :: IO ()
main = do
    (timestamp,ids) <- parseFile <$> readFile file
    cl <- return $ classify timestamp ids
    h <- return $ head cl
    putStrLn $ "Optimal choice: " ++ show h
    putStrLn $ "Product: " ++ (show $ uncurry (*) h)



