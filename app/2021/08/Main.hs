{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main where
import Lib

type Segs = String
type Evidence = [Segs] -- always 10
type Display = (Evidence,[Segs])

getDisplay :: String -> Display
getDisplay ss = let
    [e,s] = wordsWhen ((==) '|') ss
    in (words e, words s)

segsKnown :: Segs -> Bool
segsKnown ss = case length ss of
    2 -> True
    3 -> True
    4 -> True
    7 -> True
    _ -> False

reference :: Evidence
reference = [
    "abcefg",
    "cf",
    "acdeg",
    "acdfg",
    "bcdf",
    "abdfg",
    "abdefg",
    "acf",
    "abcdefg",
    "abcdfg"
    ]

-- the signature of a particular segment
type SegSignature = [Int]
getSegSignature :: Evidence -> Char -> SegSignature
getSegSignature ev c = sort $ fmap length $ filter (elem c) ev

-- the signature of a string of segments
type Signature = Set SegSignature
getSignature :: Evidence -> String -> Signature
getSignature ev s = fromList $ fmap (getSegSignature ev) s

referenceTable :: [(Signature,Int)]
referenceTable = zip (fmap (getSignature reference) reference) [0..9]

calcDisplay :: Display -> Int
calcDisplay (ev,ss) = let
    segsToDigit :: Segs -> Int
    segsToDigit segs = flookup (getSignature ev segs) referenceTable
    in (segsToDigit $ ss !! 0) * 1000 + (segsToDigit $ ss !! 1) * 100 + (segsToDigit $ ss !! 2) * 10 + (segsToDigit $ ss !! 3)

main :: IO ()
main = do
    f <- readFile "app/2021/08/input.txt"
    let
        disps :: [Display]
        disps = fmap getDisplay $ lines f
    reportPart1 $ sum $ fmap (length . filter segsKnown . snd) disps
    reportPart2 $ sum $ fmap calcDisplay disps
