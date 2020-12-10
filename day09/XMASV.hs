module XMASV where

import qualified Data.Vector as V
import Control.Monad

slice' :: (Int,Int) -> V.Vector a -> V.Vector a
slice' (l,r) v = V.slice l (r - l + 1) v

sumSlice :: Num a => (Int,Int) -> V.Vector a -> a
sumSlice ran v = V.sum $ slice' ran v

findSubSeq1 :: (Num a, Eq a, Ord a) => a -> V.Vector a -> Maybe (Int,Int)
findSubSeq1 target vec =
    increaseTo (0,1)
    where n = V.length vec
          increaseTo (i,j)
              | j >= n = Nothing
              | otherwise =
                  let s = sumSlice (i,j) vec in
                      if s == target
                          then Just (i,j)
                      else if s > target
                          then decreaseTo (i+1,j)
                      else
                          increaseTo (i,j+1)
          decreaseTo (i,j)
              | j - i < 1 = increaseTo (i+1,i+2)
              | otherwise =
                  let s = sumSlice (i,j) vec in
                      if s == target
                          then Just (i,j)
                      else if s < target
                          then increaseTo (i,j+1)
                      else
                          decreaseTo (i+1,j)

findSubSeq2 :: (Num a, Eq a, Ord a) => a -> V.Vector a -> Maybe (V.Vector a)
findSubSeq2 target vec = do
    ran <- findSubSeq1 target vec
    return $ slice' ran vec

parse :: [String] -> [Int]
parse = map read

parseFile :: FilePath -> IO [Int]
parseFile fp = readFile fp >>= (return . parse . lines)

file = "xmas.txt"
theSize = 25
target = 138879426

main :: IO ()
main = do
    nums <- parseFile file
    result <- return $ findSubSeq' target $ V.fromList nums
    putStrLn $ "Result: " ++ show result





