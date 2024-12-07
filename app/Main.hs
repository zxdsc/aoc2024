{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where
import Data.List
import qualified Data.Text as T
import GHC.Num (integerFromInt)
import Text.Regex.TDFA
import Text.Read (Lexeme(String))

main :: IO ()
main = do
   input <- getContents
   let matrix = transformInput input
   print $ calcLevels matrix
   print $ calcLevels' matrix

-- Day 1 

transform :: String -> ([Integer], [Integer])
transform s = unzip $ map splitLine $ T.splitOn "\n" (T.pack s)


splitLine :: T.Text -> (Integer, Integer)
splitLine "" = (0, 0)
splitLine t = (read $ T.unpack l, read $ T.unpack r)
    where
        [l, r] = filter (/="") $ T.splitOn " " t


calc :: ([Integer], [Integer]) -> Integer
calc (l, r) = foldr (\(sl, sr) acc -> acc + abs (sl - sr)) 0 paired
    where
       sortedL = sort l
       sortedR = sort r
       paired = zip sortedL sortedR

calcSimilarity :: ([Integer], [Integer]) -> Integer
calcSimilarity (left, right) = sum prod
    where
        individualProd n = n * integerFromInt (length $ filter (== n) right)
        prod = map individualProd left

-- Day 2

transformInput :: String -> [[Integer]]
transformInput str =
    let lines = T.splitOn "\n" $ T.pack str
        list "" = []
        list x = map (read . T.unpack) $ T.splitOn " " x
    in map list lines


calcLevels :: [[Integer]] -> Integer
calcLevels matr = integerFromInt $ length (filter id $ map calcLevel matr)

calcLevels' :: [[Integer]] -> Integer
calcLevels' matr = integerFromInt $ length (filter id $ map calcLevel' matr)



calcLevel :: [Integer] -> Bool
calcLevel [] = False
calcLevel ls =
    let difs = zipWith (\pr nxt -> let dist = nxt - pr in (dist, signum dist)) ls (tail ls)
        inInterval x = x `betweenIncluding` (1, 3)
    in all (\(dst, sig) -> inInterval (abs dst) && sig == snd (head difs)) difs

calcLevel' :: [Integer] -> Bool
calcLevel' ls = any calcLevel attemts
    where
        attemts = zipWith (++) (inits ls) (tail $ tails ls)

betweenIncluding :: Integer -> (Integer, Integer) -> Bool
betweenIncluding x (l, r)
    | x >= l && x <= r = True
    | otherwise = False

-----------------------------------------
-- Day 3 
-- --------------------------------------
day3 :: String -> IO ()
day3 pathToFile = do
    input <- readFile pathToFile
    let instructions = input =~ pattern :: [[String]]
    print $ sum [read (instruction !! 1) * read (instruction !! 2) | instruction <- instructions]

pattern :: String
pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"


data Instruction = Do | Dont | Mul (Integer, Integer) deriving (Show)

toInstruction :: String -> Instruction
toInstruction s
    | "don't" `isInfixOf` s = Dont
    | "do" `isInfixOf` s = Do
    | "mul" `isInfixOf` s = 
        let
            pat = "[0-9]{1,3}" :: String
            parsed = getAllTextMatches (s =~ pat) :: [String] 
        in Mul (read (parsed !! 0), read (parsed !! 1))
    | otherwise = undefined

enrichedPattern :: String
enrichedPattern = pattern ++ "|don't|do"

betweenDo'n'Dont :: [Instruction] -> [Instruction]
betweenDo'n'Dont ins = helper ins True []
    where
        helper :: [Instruction] -> Bool -> [Instruction] -> [Instruction]
        helper [] _ acc = acc
        helper (x: xs) f acc = case x of
            Do -> helper xs True acc
            Dont -> helper xs False acc
            _ -> if f then helper xs f (acc ++ [x]) else helper xs f acc 
         
calcMul :: Instruction -> Integer
calcMul (Mul (a,b)) = a * b

day3' :: String -> IO ()
day3' path = do
    input <- readFile path
    let pat = "mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)" :: String
    let parsed = getAllTextMatches (input =~ pat) :: [String]
    let instructions = map toInstruction parsed
    let actualInstructoins = betweenDo'n'Dont instructions
    print $ sum $ map calcMul actualInstructoins
