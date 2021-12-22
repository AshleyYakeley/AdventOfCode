{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib hiding (rangeSize)

type Point = (Int,Int,Int)

type Range = (Int,Int)

type Cuboid = (Range,Range,Range)

type Step = (Bool,Cuboid)

type P = Parser Char

parseRange :: P Range
parseRange = do
    a <- readInt
    readThese ".."
    b <- readInt
    return (a,b)

parseStep :: P Step
parseStep = do
    s <- (readThese "on" >> return True) <|> (readThese "off" >> return False)
    readThese " x="
    rx <- parseRange
    readThese ",y="
    ry <- parseRange
    readThese ",z="
    rz <- parseRange
    return (s,(rx,ry,rz))

intInRange :: Range -> Int -> Bool
intInRange (a,b) i = i >= a && i <= b

inCuboid :: Cuboid -> Point -> Bool
inCuboid (rx,ry,rz) (x,y,z) = intInRange rx x && intInRange ry y && intInRange rz z

isOn :: [Step] -> Point -> Bool -> Bool
isOn [] _ x = x
isOn ((y,c):ss) p x = isOn ss p $ if inCuboid c p then y else x

withR :: (Int -> Int) -> Int
withR f = sum $ fmap f [-50..50]

overlapRange :: Range -> Range -> Bool
overlapRange (a1,b1) (a2,b2) = a1 <= b2 && a2 <= b1

overlapCuboid :: Cuboid -> Cuboid -> Bool
overlapCuboid (rx1,ry1,rz1) (rx2,ry2,rz2) = overlapRange rx1 rx2 && overlapRange ry1 ry2 && overlapRange rz1 rz2

type Reactor = [Cuboid]

cutRange :: Int -> Range -> [Range]
cutRange i (a,b) = if i <= a then [(a,b)] else if i > b then [(a,b)] else [(a,pred i),(i,b)]

cutCuboidX :: Int -> Cuboid -> [Cuboid]
cutCuboidX i (rx,ry,rz) = fmap (\rx' -> (rx',ry,rz)) $ cutRange i rx

cutCuboidY :: Int -> Cuboid -> [Cuboid]
cutCuboidY i (rx,ry,rz) = fmap (\ry' -> (rx,ry',rz)) $ cutRange i ry

cutCuboidZ :: Int -> Cuboid -> [Cuboid]
cutCuboidZ i (rx,ry,rz) = fmap (\rz' -> (rx,ry,rz')) $ cutRange i rz

splitCuboid :: Cuboid -> Cuboid -> [Cuboid]
splitCuboid ((ax,bx),(ay,by),(az,bz)) c0 = do
    c1 <- cutCuboidX ax c0
    c2 <- cutCuboidX (succ bx) c1
    c3 <- cutCuboidY ay c2
    c4 <- cutCuboidY (succ by) c3
    c5 <- cutCuboidZ az c4
    c6 <- cutCuboidZ (succ bz) c5
    return c6

splitCuboid' :: Cuboid -> Cuboid -> [Cuboid]
splitCuboid' c1 c2 = let
    cc = splitCuboid c1 c2
    in if sum (fmap cuboidSize cc) == cuboidSize c2 then cc else error "bad split"

cutOutCuboid :: Cuboid -> Cuboid -> [Cuboid]
cutOutCuboid newc oldc = if overlapCuboid newc oldc then
    filter (\subc -> not $ overlapCuboid newc subc) $ splitCuboid' newc oldc
    else [oldc]

stepReactor :: Step -> Reactor -> Reactor
stepReactor (s,newc) r = let
    cc = mconcat $ fmap (cutOutCuboid newc) r
    in if s then newc : cc else cc

stepsReactor :: [Step] -> Reactor -> Reactor
stepsReactor [] r = r
stepsReactor (s:ss) r = stepsReactor ss $ stepReactor s r

rangeSize :: Range -> Int
rangeSize (a,b) = succ $ b - a

cuboidSize :: Cuboid -> Int
cuboidSize (rx,ry,rz) = rangeSize rx * rangeSize ry * rangeSize rz

main :: IO ()
main = do
    inputstring <- readFile "app/2021/22/input.txt"
    let
        steps :: [Step]
        steps = fmap (runParser parseStep) $ lines inputstring
        stepsInit :: [Step]
        stepsInit = take 20 steps
        getCube :: Point -> Bool
        getCube p = isOn stepsInit p False
    reportPart1 $ withR $ \x -> withR $ \y -> withR $ \z -> if getCube (x,y,z) then 1 else 0
    let
        reactor = stepsReactor steps []
    reportPart2 $ sum $ fmap cuboidSize reactor
