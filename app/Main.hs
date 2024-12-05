{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.List
import Data.String
import qualified Data.Text as T
import Control.Arrow (ArrowChoice(right))
import GHC.Num (integerFromInt)

main :: IO ()
main = do
   input <- getContents
   let lists = transform input
   let res = calc lists
   print res
   let res2 = calcSimilarity lists
   print res2

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
