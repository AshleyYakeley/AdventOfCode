{-# OPTIONS -Wno-incomplete-patterns #-}
module Main where
import Lib

main :: IO ()
main = do
    f <- readFile "app/2021/07/input.txt"
    let
        crabs :: [Int]
        crabs = fmap read $ wordsWhen ((==) ',') f
        minfuel :: (Int -> Int) -> Int
        minfuel df = minimum $ fmap (\p -> sum $ fmap (\crab -> df $ abs $ p - crab) crabs) [0..maximum crabs]
    reportPart1 $ minfuel id
    reportPart2 $ minfuel $ \d -> d * (d + 1) `div` 2
