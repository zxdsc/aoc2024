{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.List
import Data.String
import qualified Data.Text as T
import Control.Arrow (ArrowChoice(right))
import GHC.Num (integerFromInt)
import Data.Function (on)

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


