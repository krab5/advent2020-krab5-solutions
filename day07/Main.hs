{-
   Challenge Day 7: combinatorials and exhaustive search
   wou-hou!!!!
   First part is done with a stupid bruteforce algorithm, second
   part is "slightly" clever and builds a tree from the set of
   rules.

   (yes I could have re-used this tree structure for the first part
   but brute-force + lazyness (of the language) is fun).

   Big note: the puzzle specified does not count the shiny gold bag
   as part of the solution set. I do. Don't forget to do a "- 1" on
   both results to get the correct result.
-}
module Main where

import Luggage.Parser
import Luggage.Rules
import Luggage.Tree
import Control.Monad
import qualified Data.Set as S

-- problem's data
infile = "rules.txt"
seed = mkcolor "shiny" "gold"
 
main :: IO ()
main = do
    rawrules <- parse <$> readFile infile
    rules <- return $ reduceAll rawrules
    let s = backk rules $ S.singleton seed in do
        putStrLn $ "Number of bags (INCLUDING THE SHINY GOLD): " ++ (show $ S.size s)
        putStrLn $ show s
        let tree = make_tree rawrules seed in
            putStrLn $ "Number of bags for shiny gold (INCLUDING THE SHINY GOLD): " ++ (show $ sum_tree tree)


