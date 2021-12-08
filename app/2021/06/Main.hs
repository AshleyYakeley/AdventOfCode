{-# OPTIONS -Wno-incomplete-patterns #-}
module Main where
import Lib

type Fish = [Int]

initialFish :: [Int] -> Fish
initialFish ff = fmap (\i -> length $ filter ((==) i) ff) [0..8]

evolve :: Fish -> Fish
evolve (f:ff) = zipWith (+) [0,0,0, 0,0,0, f,0,0] ff <> [f]

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN 0 _ = id
iterateN n f = f . iterateN (pred n) f

main :: IO ()
main = do
    f <- readFile "app/2021/06/input.txt"
    let
        fish :: Fish
        fish = initialFish $ fmap read $ wordsWhen ((==) ',') f
    reportPart1 $ sum $ iterateN 80 evolve fish
    reportPart2 $ sum $ iterateN 256 evolve fish
