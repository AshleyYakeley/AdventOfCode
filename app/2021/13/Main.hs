{-# OPTIONS -Wno-incomplete-patterns #-}
module Main(main) where
import Lib

type Point = (Int,Int)

getPoint :: String -> Maybe Point
getPoint s = case wordsWhen (== ',') s of
    [x,y] -> Just (read x,read y)
    _ -> Nothing

type Fold = (Bool,Int)

getFold :: String -> Maybe Fold
getFold s = do
    s1 <- startsWith "fold along " s
    case s1 of
        'x':'=':s2 -> Just (False,read s2)
        'y':'=':s2 -> Just (True,read s2)

type Paper = [Point]

doFoldPoint :: Fold -> Point -> Point
doFoldPoint (False,n) (x,y) = (if x > n then 2*n - x else x,y)
doFoldPoint (True ,n) (x,y) = (x,if y > n then 2*n - y else y)

doFold :: Paper -> Fold -> Paper
doFold paper f = nub $ fmap (doFoldPoint f) paper

showPaper :: Paper -> [String]
showPaper pp = let
    maxX = succ $ maximum $ fmap fst pp
    maxY = succ $ maximum $ fmap snd pp
    s0 = replicate maxY $ replicate maxX '.'
    in foldr (\(x,y) s -> modifyList y (modifyList x $ \_ -> '#') s) s0 pp

main :: IO ()
main = do
    inputstring <- readFile "app/2021/13/input.txt"
    let
        ll = lines inputstring
        paper = mapMaybe getPoint ll
        folds = mapMaybe getFold ll

        folded1 = doFold paper $ head folds
        folded = foldl doFold paper folds
    reportPart1 $ length folded1
    for_ (showPaper folded) reportPart2
