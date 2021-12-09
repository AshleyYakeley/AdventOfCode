module Main where
import Lib

type Point = (Int,Int)

main :: IO ()
main = do
    filestring <- readFile "app/2021/09/input.txt"
    let
        heightMap :: [[Int]]
        heightMap = fmap (fmap (read . pure)) $ lines filestring

        height :: Point -> Int
        height (x,_) | x < 0 = maxBound
        height (x,_) | x >= 100 = maxBound
        height (_,y) | y < 0 = maxBound
        height (_,y) | y >= 100 = maxBound
        height (x,y) = heightMap !! y !! x

        allPoints :: [Point]
        allPoints = [(x,y) | x <- [0..99], y <- [0..99]]

        neighbours :: Point -> [(Point,Int)]
        neighbours (x,y) = let
            n = [(pred x,y), (succ x,y), (x,pred y), (x,succ y)]
            in fmap (\p -> (p,height p)) n

        lowHeight :: Point -> Maybe Int
        lowHeight p0 = let
            h0 = height p0
            in if all (\(_,h) -> h0 < h) $ neighbours p0
                then Just $ succ h0
                else Nothing

        basinMove :: Point -> Maybe Point
        basinMove p0 = let
            h0 = height p0
            in if h0 == 9
                then Nothing else Just $ case headM $ filter ((< h0) . snd) $ neighbours p0 of
                    Just (p,_) -> p
                    Nothing -> p0

        basinLimit :: Point -> Maybe Point
        basinLimit p = do
            pp <- basinMove p
            if pp == p
                then return p
                else basinLimit pp

        basinMap :: [Point]
        basinMap = mapMaybe basinLimit allPoints

    reportPart1 $ sum $ mapMaybe lowHeight allPoints
    reportPart2 $ product $ take 3 $ reverse $ sort $ fmap (\p -> length $ filter (== p) basinMap) $ nub basinMap
