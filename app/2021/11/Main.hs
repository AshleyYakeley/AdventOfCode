module Main where
import Lib

type Point = (Int,Int)

type Board = IOUArray Point Int

points :: [Point]
points = [(x,y) | x <- [0..9], y <- [0..9]]

neighbours :: Point -> [Point]
neighbours (x,y) = [
    (pred x,pred y),(x,pred y),(succ x,pred y),
    (pred x, y),               (succ x, y),
    (pred x,succ y),(x,succ y),(succ x,succ y)
    ]

main :: IO ()
main = do
    inputstring <- readFile "app/2021/11/input.txt"
    let
        newBoard :: IO Board
        newBoard = do
            board <- newArray ((0 :: Int,0 :: Int),(9,9)) (0 :: Int)
            for_ (zip [0..] $ lines inputstring) $ \(y,line) -> for_ (zip [0..] line) $ \(x,c) -> writeArray board (x,y) $ read [c]
            return board

        doStep :: Board -> IO Int
        doStep board = let
            modifyPoint :: Point -> (Int -> Int) -> IO ()
            modifyPoint p@(x,y) f | x >= 0 && x < 10 && y >= 0 && y < 10 = do
                e <- readArray board p
                writeArray board p $ f e
            modifyPoint _ _ = return ()

            doFlashes :: IO Int
            doFlashes = do
                flist <- for points $ \p -> do
                    e <- readArray board p
                    if e > 9 then do
                        for_ (neighbours p) $ \n -> modifyPoint n (\ne -> if ne > 0 then succ ne else 0)
                        writeArray board p 0
                        return 1
                    else return 0
                let
                    f = sum flist
                if f > 0
                    then fmap (+ f) doFlashes
                    else return 0
            in do
                for_ points $ \p -> modifyPoint p succ
                doFlashes

    do
        board <- newBoard
        flashes <- for [1 :: Int ..100] $ \_ -> doStep board
        reportPart1 $ sum flashes

    do
        board <- newBoard
        let
            waitForSync :: Int -> IO Int
            waitForSync n = do
                c <- doStep board
                if c == 100 then return n else waitForSync $ succ n
        n <- waitForSync 1
        reportPart2 n
