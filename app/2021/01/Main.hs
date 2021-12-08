module Main where
import Lib

solve1 :: [Int] -> Int
solve1 xx = length $ filter id $ zipWith (>) xx $ maxBound : xx

solve2 :: [Int] -> Int
solve2 xx = length $ filter id $ zipWith (>) xx $ maxBound : maxBound : maxBound : xx

main :: IO ()
main = do
    f <- readFile "app/2021/01/input.txt"
    let xx = fmap read $ lines f
    reportPart1 $ solve1 xx
    reportPart2 $ solve2 xx
