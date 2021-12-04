module Main where

solve1 :: [Int] -> Int
solve1 xx = length $ filter id $ zipWith (>) xx $ maxBound : xx

solve2 :: [Int] -> Int
solve2 xx = length $ filter id $ zipWith (>) xx $ maxBound : maxBound : maxBound : xx

main :: IO ()
main = do
    f <- readFile "app/2021/01/input.txt"
    let xx = fmap read $ lines f
    putStrLn $ "Part 1: " <> show (solve1 xx)
    putStrLn $ "Part 2: " <> show (solve2 xx)
