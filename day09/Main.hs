module Main where

import XMAS
import qualified Data.Vector as V

parse :: [String] -> [Int]
parse = map read

parseFile :: FilePath -> IO [Int]
parseFile fp = readFile fp >>= (return . parse . lines)

file = "xmas.txt"
theSize = 25

main :: IO ()
main = do
    nums <- parseFile file
    (pre,suff) <- return $ break (not . fst) $ nom theSize nums
    if null suff then putStrLn "No num found :("
                 else putStrLn $ "Number found! " ++ show (head suff)
    target <- return $ snd $ head suff
    preffix <- return $ takeWhile (/= target) nums
    --answers <- return $ findSubSeq target preffix  -- Inefficient version
    answers <- return $ V.toList <$> (findSubSeq2 target $ V.fromList preffix)
    case answers of
      Nothing -> putStrLn "No answer found :("
      Just l -> do
          putStrLn "Possible answer:"
          putStrLn $ show l
          let (tmin,tmax) = (minimum l, maximum l) in
              putStrLn $ "  (min=" ++ show tmin ++ ",max=" ++ show tmax ++ ",sum=" ++ show (tmin + tmax) ++ ")"




