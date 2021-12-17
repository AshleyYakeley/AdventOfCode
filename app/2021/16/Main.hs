{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

f :: Bool
f = False

t :: Bool
t = True

getBits :: Char -> [Bool]
getBits '0' = [f,f,f,f]
getBits '1' = [f,f,f,t]
getBits '2' = [f,f,t,f]
getBits '3' = [f,f,t,t]
getBits '4' = [f,t,f,f]
getBits '5' = [f,t,f,t]
getBits '6' = [f,t,t,f]
getBits '7' = [f,t,t,t]
getBits '8' = [t,f,f,f]
getBits '9' = [t,f,f,t]
getBits 'A' = [t,f,t,f]
getBits 'B' = [t,f,t,t]
getBits 'C' = [t,t,f,f]
getBits 'D' = [t,t,f,t]
getBits 'E' = [t,t,t,f]
getBits 'F' = [t,t,t,t]
getBits _ = []

bit :: Bool -> Int
bit False = 0
bit True = 1

type P = StateT [Bool] Maybe

rbit :: P Bool
rbit = StateT $ \case
    [] -> Nothing
    (s:ss) -> Just (s,ss)

rbiti :: P Int
rbiti = fmap bit rbit

rbits :: Int -> P Int
rbits 0 = return 0
rbits n = do
    x <- rbits $ pred n
    y <- rbiti
    return $ x * 2 + y

readLiteral :: Int -> P Int
readLiteral x = do
    c <- rbit
    i <- rbits 4
    if c
        then readLiteral $ (x + i) * 16
        else return $ x + i

ntimes :: Applicative m => Int -> m a -> m [a]
ntimes 0 _ = pure []
ntimes i ma = liftA2 (:) ma $ ntimes (pred i) ma

localBits :: Int -> P a -> P a
localBits n (StateT sma) = StateT $ \olds -> do
    (a,news) <- sma $ take n olds
    return (a,news <> drop n olds)

untilEmpty :: P a -> P [a]
untilEmpty pa = do
    s <- get
    case s of
        [] -> return []
        (_:_) -> liftA2 (:) pa $ untilEmpty pa

data Packet = MkPacket Int (Either Int (Int,[Packet]))

packetVerTotal :: Packet -> Int
packetVerTotal (MkPacket v ipp) = v + case ipp of
    Left _ -> 0
    Right (_,pp) -> sum $ fmap packetVerTotal pp

readPacket :: P Packet
readPacket = do
    ver <- rbits 3
    tp <- rbits 3
    fmap (MkPacket ver) $ case tp of
        4 -> fmap Left $ readLiteral 0
        _ -> do
            lti <- rbit
            pp <- if lti
                then do
                    n <- rbits 11
                    ntimes n readPacket
                else do
                    bc <- rbits 15
                    localBits bc $ untilEmpty readPacket
            return $ Right (tp,pp)

packetCalc :: Packet -> Int
packetCalc (MkPacket _ (Left i)) = i
packetCalc (MkPacket _ (Right (tp,pp))) = case tp of
    0 -> sum $ fmap packetCalc pp
    1 -> product $ fmap packetCalc pp
    2 -> minimum $ fmap packetCalc pp
    3 -> maximum $ fmap packetCalc pp
    5 -> let [a,b] = pp in bit $ packetCalc a > packetCalc b
    6 -> let [a,b] = pp in bit $ packetCalc a < packetCalc b
    7 -> let [a,b] = pp in bit $ packetCalc a == packetCalc b

main :: IO ()
main = do
    inputstring <- readFile "app/2021/16/input.txt"
    let
        rawBits :: [Bool]
        rawBits = mconcat $ fmap getBits  inputstring
        Just (p,_) = runStateT readPacket rawBits

    reportPart1 $ packetVerTotal p
    reportPart2 $ packetCalc p
