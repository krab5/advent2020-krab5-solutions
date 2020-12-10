{-
   Specification for luggage rules.
-}
module Luggage.Rules where

import qualified Data.Set as S

-- A color could be a more refined type but this seems
-- unnecessary for this challenge
type Color = String

{-
   A rule "fragment", i.e. a "n color bags" on the "right hand
   side" of the rule.
-}
data RuleFragment = RuleFragment Color Int

{-
   A rule, representing that a bag of the given color must
   contain all the bags specified by the fragments.
-}
data Rule = Rule Color [RuleFragment]

{-
   Convenient Show instance for the fragments.
-}
instance Show RuleFragment where
  show (RuleFragment col n) = col ++ "×" ++ show n

{-
   Convenient Show instance for the rules.
-}
instance Show Rule where
  show (Rule col fr) = col ++ " -> " ++ show fr

{-
   A "mono" rule is a simple
   > <color> contains <num> <color>
   rule. Any rule is equivalent to a finite set of 
   mono rules, and they are easier to apply!
-}
data MonoRule = MonoRule {
        pre :: String,
        qty :: Int,
        post :: String
    } deriving (Eq,Ord) -- This derived instances are crucial for using sets of monorules

{-
   Convenient Show instance for mono rules.
-}
instance Show MonoRule where
  show (MonoRule container n contained) = container ++ " -(" ++ show n ++ ")-> " ++ contained

{-
   Transform a rule into a set of mono rules.
-}
reduce :: Rule -> S.Set MonoRule
reduce (Rule container frag) =
    S.fromList $ map (\(RuleFragment contained n) -> MonoRule container n contained) frag

{-
   Transform a list of rules into a set of mono rules.*
-}
reduceAll :: [Rule] -> S.Set MonoRule
reduceAll =
    foldl next S.empty
    where next acc x = S.union acc $ reduce x

{-
   Ha ha. Incoming stupid closure-type bruteforcing HELL YEAH.
   Back guesses the possible antecedents ("pre") of a set of rules
   which execution yields the given target ("post", here called str).
   
   Basically, if you have rules like:
   > orange -> 1 green
   > green -> 1 blue
   > blue -> 2 greens
   Then
   > back {} green = { orange, blue }
-}
back :: S.Set MonoRule -> String -> S.Set String
back s str =
    S.insert str $ S.map pre $ S.filter checks s
    where checks rule = post rule == str

{-
   Apply "back" on a set of words and return an union of
   the results.
-}
backAll :: S.Set MonoRule -> S.Set String -> S.Set String
backAll s strs =
    S.unions $ S.map (back s) strs

{-
   Ha ha. Fix point. Dangerous. Don't do that kids.
   This iterate a function until two consecutive values are
   identical...
   This is awful as
   - it may not terminate
   - comparison operation may be slow (imagine comparing marge sets...)
-}
fix :: Eq a => (a -> a) -> a -> a
fix f x =
    stop $ iterate f x
    where stop (x1:x2:xs)
            | x1 == x2 = x1
            | otherwise = stop (x2:xs)

{-
   Backward rules applications...
   Back-apply the rules from a starting set (a "seed") until
   nothing progresses anymore ("fixpoin").
   In fine, we now this fixpoint exists because rules are not 
   destructive: any application of rules adds, and never removes,
   meaning that, if f = backAll rules
   > x ⊆ f x
   And there exist a finite, maximal super set (of all colors available
   in the rules) => oof! We're saved; this function finishes.
-}
backk :: S.Set MonoRule -> S.Set String -> S.Set String
backk rules seed = fix (backAll rules) seed




