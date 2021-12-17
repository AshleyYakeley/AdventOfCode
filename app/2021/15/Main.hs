{-# OPTIONS -Wno-incomplete-patterns #-}
module Main(main) where
import Lib

type CMap = Array (Int,Int) Int

type MinMap = IOUArray (Int,Int) Int

solve :: CMap -> IO Int
solve cmap = do
    let
        ((xmin,ymin),(xmax,ymax)) = bounds cmap
    minmap :: MinMap <- newArray ((xmin,ymin),(xmax,ymax)) maxBound
    let
        step :: IO Bool
        step = do
            flag <- newIORef False
            for_ [ymin..ymax] $ \y -> for_ [xmin..xmax] $ \x -> do
                newv <- if x == xmin && y == ymin then return 0 else do
                    v1 <- if x > xmin then readArray minmap (pred x,y) else return maxBound
                    v2 <- if y > ymin then readArray minmap (x,pred y) else return maxBound
                    v3 <- if x < xmax then readArray minmap (succ x,y) else return maxBound
                    v4 <- if y < ymax then readArray minmap (x,succ y) else return maxBound
                    return $ (minimum [v1,v2,v3,v4]) + cmap ! (x,y)
                oldv <- readArray minmap (x,y)
                if oldv == newv then return () else do
                    writeArray minmap (x,y) newv
                    writeIORef flag True
            readIORef flag

    while step
    readArray minmap (xmax,ymax)

main :: IO ()
main = do
    inputstring <- readFile "app/2021/15/input.txt"
    let
        smallmap :: CMap
        smallmap = listArray ((0,0),(99,99)) $ mconcat $ fmap (fmap $ \c -> read [c]) $ lines inputstring

        bigmap :: CMap
        bigmap = array ((0,0),(499,499)) [
            ((xx * 100 + x, yy * 100 + y),(((smallmap ! (x,y)) + xx + yy - 1) `mod` 9) + 1) |
            xx <- [0..4],
            yy <- [0..4],
            x <- [0..99],
            y <- [0..99]
            ]

    solve smallmap >>= reportPart1
    solve bigmap >>= reportPart2
