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

type Fish = [Int]

initialFish :: [Int] -> Fish
initialFish ff = fmap (\i -> length $ filter ((==) i) ff) [0..8]

evolve :: Fish -> Fish
evolve (f:ff) = zipWith (+) [0,0,0, 0,0,0, f,0,0] ff <> [f]

iterate :: Int -> (a -> a) -> (a -> a)
iterate 0 _ = id
iterate n f = f . iterate (pred n) f

main :: IO ()
main = do
    f <- readFile "app/2021/06/input.txt"
    let
        fish :: Fish
        fish = initialFish $ fmap read $ wordsWhen ((==) ',') f
    putStrLn $ "Part 1: " <> (show $ sum $ iterate 80 evolve fish)
    putStrLn $ "Part 2: " <> (show $ sum $ iterate 256 evolve fish)
