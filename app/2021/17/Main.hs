{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

type Point = (Int,Int)
type S = (Point,Point,Int)

step :: S -> S
step ((x,y),(vx,vy),maxy) = ((x + vx,y + vy),(vx - signum vx,pred vy), max y maxy)

type Target = (Point,Point)

hit :: Target -> Point -> Bool
hit ((xmin,ymin),(xmax,ymax)) (x,y) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

missed :: Target -> Point -> Bool
missed ((_,ymin),(xmax,_)) (x,y) = x > xmax || y < ymin

run :: Target -> S -> Maybe Int
run t (p,_,_) | missed t p = Nothing
run t (p,_,maxy) | hit t p = Just maxy
run t s = run t $ step s

shot :: Target -> Point -> Maybe Int
shot t p = run t ((0,0),p,0)

main :: IO ()
main = do
    inputstring <- readFile "app/2021/17/input.txt"
    let
        [read -> xmin,read -> xmax,read -> ymin,read -> ymax] = getMatches "target area: x=(.+)\\.\\.(.+), y=(.+)\\.\\.(.+)" inputstring
        target = ((xmin,ymin),(xmax,ymax))
        vels :: [Point]
        vels = [(x,y) | x <- [1..xmax],y <- [ymin..10000]]
        passes = mapMaybe (shot target) vels

    reportPart1 $ maximum passes
    reportPart2 $ length passes
