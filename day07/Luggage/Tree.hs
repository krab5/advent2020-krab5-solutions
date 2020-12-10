{-
   A simple n-ary tree with colors in the nodes and
   weights in the branches.
-}
module Luggage.Tree where

import Luggage.Rules
import qualified Data.Set as S

{-
   Tree types: branches have a weight, nodes have n childrens
   and a word (color) in the root.
-}
data Branch = Branch { weight :: Int, node :: Node } deriving Show
data Node = Node { root :: String, children :: [Branch] } deriving Show

{-
   Transform a set of rules + a root into a node,
   effectively crafting a tree.
-}
make_tree :: [Rule] -> String -> Node
make_tree rules root =
    Node root branches
    where rules_with_pre = filter check rules
          check (Rule source _) = source == root
          branches = foldl (++) [] $ map (\(Rule _ frags) -> map make_branch frags) rules_with_pre
          make_branch (RuleFragment col qty) = Branch qty $ make_tree rules col

{-
   Perform a distribution sum on the tree: this is the
   sum of every subnode, weighted by their branch's weight.
-}
sum_tree :: Node -> Int
sum_tree root =
    foldl (\acc b -> acc + sum_branch b) 1 $ children root
    where sum_branch :: Branch -> Int
          sum_branch br = (weight br) * (sum_tree $ node br)




