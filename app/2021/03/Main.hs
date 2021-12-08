module Main where
import Lib

type Bit = Int
type D = [Bit]
type Index = Int

dNum :: D -> Int
dNum ii = let
    f n [] = n
    f n (a : aa) = f (n * 2 + a) aa
    in f 0 ii

getCommonestBit :: Bool -> Index -> [D] -> Bit
getCommonestBit more i dd = let
    ii = fmap (\d -> d !! i) dd
    in if ((2 * sum ii) >= length dd) == more then 1 else 0

reduceSet :: Bool -> Index -> [D] -> [D]
reduceSet more i dd = let
    bit = getCommonestBit more i dd
    in filter (\d -> d !! i == bit) dd

reduced :: Bool -> Index -> [D] -> D
reduced more i dd = case dd of
    [] -> error "empty"
    [d] -> d
    _ -> reduced more (succ i) $ reduceSet more i dd

main :: IO ()
main = do
    f <- readFile "app/2021/03/input.txt"
    let
        dd0 :: [D]
        dd0 = fmap (fmap (\c -> if c == '1' then 1 else 0)) $ lines f
        gamma = dNum $ fmap (\i -> getCommonestBit True i dd0) [0..11]
        epsilon = dNum $ fmap (\i -> getCommonestBit False i dd0) [0..11]
    reportPart1 $ gamma * epsilon
    let
        o2 = dNum $ reduced True 0 dd0
        co2 = dNum $ reduced False 0 dd0
    reportPart2 $ o2 * co2
