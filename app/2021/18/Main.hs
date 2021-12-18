{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

data T = NumT Int | PairT T T

instance Show T where
    show (NumT i) = show i
    show (PairT a b) = "[" <> show a <> "," <> show b <> "]"

type P = StateT String Maybe

parseC :: P Char
parseC  = StateT $ \case
    (c:cc) -> Just (c,cc)
    [] -> Nothing

parseDigit :: P Char
parseDigit = do
    c <- parseC
    if c >= '0' && c <= '9' then return c else empty

parseThis :: Char -> P ()
parseThis c = do
    c' <- parseC
    if c == c' then return () else empty

parseInt :: P T
parseInt = do
    s <- some parseDigit
    return $ NumT $ read s

parsePair :: P T
parsePair = do
    parseThis '['
    t1 <- parseT
    parseThis ','
    t2 <- parseT
    parseThis ']'
    return $ PairT t1 t2

parseT :: P T
parseT = parseInt <|> parsePair

data D = L | R deriving (Show)

type Pos = [D]

findFirstNestedPair :: Int -> T -> Pos -> Maybe (Pos,Int,Int)
findFirstNestedPair _ (NumT _) _ = Nothing
findFirstNestedPair i (PairT (NumT a) (NumT b)) p | i >= 4 = Just (p,a,b)
findFirstNestedPair i (PairT a b) p = findFirstNestedPair (succ i) a (p <> [L]) <|> findFirstNestedPair (succ i) b (p <> [R])

leftPos :: T -> Pos -> Maybe Pos
leftPos (PairT a _) (L:pp) = fmap (\p -> L : p) $ leftPos a pp
leftPos (PairT a b) (R:pp) = Just $ case leftPos b pp of
    Just p' -> R : p'
    Nothing -> L : lastPos a
leftPos _ _ = Nothing

lastPos :: T -> Pos
lastPos (NumT _) = []
lastPos (PairT _ b) = R : lastPos b

rightPos :: T -> Pos -> Maybe Pos
rightPos (PairT _ b) (R:pp) = fmap (\p -> R : p) $ rightPos b pp
rightPos (PairT a b) (L:pp) = Just $ case rightPos a pp of
    Just p' -> L : p'
    Nothing -> R : firstPos b
rightPos _ _ = Nothing

firstPos :: T -> Pos
firstPos (NumT _) = []
firstPos (PairT a _) = L : firstPos a

replacePos :: Pos -> (T -> T) -> T -> T
replacePos [] f t = f t
replacePos (L:pp) f (PairT a b) = PairT (replacePos pp f a) b
replacePos (R:pp) f (PairT a b) = PairT a (replacePos pp f b)
replacePos _ _ _ = error "bad pos"

explodeT :: T -> Maybe T
explodeT t = case findFirstNestedPair 0 t [] of
    Just (p,a,b) -> let
        mlp = leftPos t p
        mrp = rightPos t p
        repL (NumT x) = NumT $ x + a
        repR (NumT x) = NumT $ x + b
        t1 :: T
        t1 = replacePos p (\_ -> NumT 0) t
        t2 = case mlp of
            Just lp -> replacePos lp repL t1
            Nothing -> t1
        t3 = case mrp of
            Just rp -> replacePos rp repR t2
            Nothing -> t2
        in Just t3
    Nothing -> Nothing

splitT :: T -> Maybe T
splitT (NumT i) | i >= 10 = Just $ PairT (NumT $ div i 2) (NumT $ div (succ i) 2)
splitT (NumT _) = Nothing
splitT (PairT a b) = fmap (\a' -> PairT a' b) (splitT a) <|> fmap (\b' -> PairT a b' ) (splitT b)

stepT :: T -> Maybe T
stepT t = explodeT t <|> splitT t

reduceT :: T -> T
reduceT t = case stepT t of
    Just t' -> reduceT t'
    Nothing -> t

addT :: T -> T -> T
addT a b = reduceT $ PairT a b

sumAllT :: T -> [T] -> T
sumAllT t [] = t
sumAllT a (b:tt) = sumAllT (addT a b) tt

sumT :: [T] -> T
sumT (t:tt) = sumAllT t tt

magnitudeT :: T -> Int
magnitudeT (NumT i) = i
magnitudeT (PairT a b) = 3 * (magnitudeT a) + 2 * (magnitudeT b)

main :: IO ()
main = do
    inputstring <- readFile "app/2021/18/input.txt"
    let
        tt :: [T]
        tt = mapMaybe (fmap fst . runStateT parseT) $ lines inputstring

    reportPart1 $ magnitudeT $ sumT tt
    reportPart2 $ maximum $ fmap (\(a,b) -> magnitudeT $ addT a b) [(a,b) | a <- tt,b <- tt]
