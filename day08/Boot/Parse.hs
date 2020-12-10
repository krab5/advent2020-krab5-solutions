{-
   Small parser for the micro assembly language thing.
-}
module Boot.Parse where

import qualified Data.Vector as V
import Text.Read
import Boot.Inst

{-
   Parse an instruction.
   Any instruction is of the form
   > <op> (+|-)?<arg>
   In theory the sign is mandatory but I made it optional.

   Weird Haskell caveat: "+5" is not readable. The correct form
   is "5" (i.e. prefix "+" is not legal).

   We use Maybe as a monad to propagate Nothings.
-}
parseInst :: String -> Maybe Inst
parseInst [] = Nothing
parseInst str = do
    op  <- parseOp inst
    arg <- parseArg $ chomp rem
    return $ (op $ arg)
    where (inst,rem) = span (/= ' ') str
          -- Return the correct constructor associated with op keyword
          parseOp "acc" = Just (Acc)
          parseOp "jmp" = Just (Jmp)
          parseOp "nop" = Just (Nop)
          parseOp _ = Nothing
          -- Parse argument (with the weird "+" caveat)
          parseArg ('+':xs) = readMaybe xs
          parseArg xs       = readMaybe xs
          chomp = dropWhile (== ' ')

{-
   Parse a list of strings into a list of instructions, or
   a bunch of error message to debug (unnecessary in theory but 
   useful to have...
-}
parseInsts :: [String] -> Either String [Inst]
parseInsts lines =
    reverse <$> (foldl parseAndCheck (Right []) $ zip [1..] lines)
    where parseAndCheck :: Either String [Inst] -> (Int,String) -> Either String [Inst]
          parseAndCheck acc (n,x) =
            case parseInst x of
              Nothing -> report acc (n,x)
              Just xp -> accum acc xp
          report :: Either String [Inst] -> (Int,String) -> Either String [Inst]
          report (Left xs) (n,x) = Left $ xs ++ report1 (n,x)
          report _         (n,x) = Left $ report1 (n,x)
          report1 (n,x) = "Line " ++ show n ++ ": syntax error, '" ++ x ++ "'\n"
          accum :: Either String [Inst] -> Inst -> Either String [Inst]
          accum acc xp = acc >>= (return . (xp:))

{-
   Parse a file full of instructions.
-}
parseFile :: FilePath -> IO Insts
parseFile fp = do
    ls <- lines <$> readFile fp
    case parseInsts ls of
      Left err -> do
          putStrLn $ "Error were reported while parsing file '" ++ fp ++ "':\n" ++ err
          return $ V.empty
      Right res -> do
          putStrLn $ "File '" ++ fp ++ "' parsed succesfully."
          return $ V.fromList res




