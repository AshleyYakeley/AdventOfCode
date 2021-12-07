{-# OPTIONS -Wno-incomplete-patterns #-}
module Main where
import Data.Foldable
import Prelude hiding (iterate)

-- copied from https://stackoverflow.com/a/4981265
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main :: IO ()
main = do
    f <- readFile "app/2021/07/input.txt"
    let
        crabs :: [Int]
        crabs = fmap read $ wordsWhen ((==) ',') f
        minfuel :: (Int -> Int) -> Int
        minfuel df = minimum $ fmap (\p -> sum $ fmap (\crab -> df $ abs $ p - crab) crabs) [0..maximum crabs]
    putStrLn $ "Part 1: " <> (show $ minfuel id)
    putStrLn $ "Part 2: " <> (show $ minfuel $ \d -> d * (d + 1) `div` 2)
