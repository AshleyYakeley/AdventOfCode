{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main where
import Lib

type Point = (Int,Int)

type Vent = (Point,Point)

readVent :: String -> Vent
readVent s = let
    [p1,_,p2] = words s
    [x1,y1] = fmap read $ wordsWhen ((==) ',') p1
    [x2,y2] = fmap read $ wordsWhen ((==) ',') p2
    in ((x1,y1),(x2,y2))

type Board = IOUArray Point Int

maxi :: Int
maxi = 999

newBoard :: IO Board
newBoard = newArray ((0,0),(maxi,maxi)) 0

urange :: Int -> Int -> [Int]
urange a b | b >= a = [a..b]
urange a b = reverse [b..a]

ventPoints :: Bool -> Vent -> [Point]
ventPoints countDiag ((x1,y1),(x2,y2)) = let
    xx = urange x1 x2
    yy = urange y1 y2
    in if x1 == x2 || y1 == y2
        then liftA2 (,) xx yy
        else if countDiag then zip xx yy else []

ventBoard :: Bool -> Vent -> Board -> IO ()
ventBoard countDiag v board = for_ (ventPoints countDiag v) $ \p -> do
    n <- readArray board p
    writeArray board p $ succ n

countBoard :: Board -> IO Int
countBoard board = do
    count <- newIORef 0
    for_ [0..maxi] $ \x ->
        for_ [0..maxi] $ \y -> do
            b <- readArray board (x,y)
            if b > 1 then modifyIORef count succ else return ()
    readIORef count

main :: IO ()
main = do
    f <- readFile "app/2021/05/input.txt"
    let
        vents = fmap readVent $ lines f
    do
        board <- newBoard
        for_ vents $ \v -> ventBoard False v board
        n <- countBoard board
        reportPart1 n
    do
        board <- newBoard
        for_ vents $ \v -> ventBoard True v board
        n <- countBoard board
        reportPart2 n
