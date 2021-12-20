{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

readChar :: Char -> Bool
readChar '.' = False
readChar '#' = True

type Point = (Int,Int)

type Scan = (Bool,Array Point Bool)

readScan :: [String] -> Scan
readScan ss = let
    arr = listArray ((0,0),(pred $ length ss,pred $ length $ ss !! 0)) $ fmap readChar $ mconcat ss
    in (False,arr)

type Alg = [Bool] -> Bool

readAlg :: String -> Alg
readAlg s = let
    d :: [Bool]
    d = fmap readChar s

    bitsToInt :: [Bool] -> Int -> Int
    bitsToInt [] i = i
    bitsToInt (False:bb) i = bitsToInt bb (i * 2)
    bitsToInt (True:bb) i =  bitsToInt bb (succ $ i * 2)
    in \bb -> d !! bitsToInt bb 0

getPoint :: Scan -> (Int, Int) -> Bool
getPoint (p0,arr) i = if inRange (bounds arr) i then arr ! i else p0

scanPoint :: Scan -> (Int, Int) -> [Bool]
scanPoint scan (r,c) = fmap (getPoint scan)
    [(r',c')| r' <- [pred r,r,succ r], c' <- [pred c,c,succ c]]

runAlg :: Alg -> Scan -> Scan
runAlg alg scan@(p0,arr) = let
    ((rmin,cmin),(rmax,cmax)) = bounds arr
    arr' :: Array Point Bool
    arr' = makeArray ((pred rmin,pred cmin),(succ rmax,succ cmax)) $ \i -> alg $ scanPoint scan i
    in (alg [p0,p0,p0,p0,p0,p0,p0,p0,p0],arr')

countScan :: Scan -> Int
countScan (False,arr) = length $ filter id $ toList arr

main :: IO ()
main = do
    inputstring <- readFile "app/2021/20/input.txt"
    let
        algt:"":scant = lines inputstring
        alg :: Alg
        alg = readAlg algt
        scan0 :: Scan
        scan0 = readScan scant

        scanN :: Int -> Scan
        scanN 0 = scan0
        scanN n = runAlg alg $ scanN (pred n)

    reportPart1 $ countScan $ scanN 2
    reportPart2 $ countScan $ scanN 50
