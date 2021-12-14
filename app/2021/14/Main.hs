{-# OPTIONS -Wno-incomplete-patterns #-}
module Main(main) where
import Lib

type Rule = (Char,Char,Char)

parseRule :: String -> Maybe Rule
parseRule [a,b,' ','-','>',' ',c] = Just (a,b,c)
parseRule _ = Nothing


-- Part 1

newtype Transform = MkTransform {unRule ::Char -> Char -> Maybe Char}

instance Semigroup Transform where
    MkTransform p <> MkTransform q = MkTransform $ \a b -> (p a b) <|> q a b

instance Monoid Transform where
    mempty = MkTransform $ \_ _ -> Nothing

getTransform :: Rule -> Transform
getTransform (a,b,c) = MkTransform $ \a' b' -> if a == a' && b == b' then Just c else Nothing

doTransform :: Transform -> String -> String
doTransform rule (a:b:s) | Just c <- unRule rule a b = a:c:(doTransform rule $ b:s)
doTransform rule (a:s) = a:(doTransform rule s)
doTransform _ [] = []

countElements :: String -> [Int]
countElements s = fmap (\c -> length $ filter (==c) s) $ nub s


-- Part 2

newtype Counts = MkCounts (Map Char Int)

singleCounts :: Char -> Counts
singleCounts c = MkCounts $ singleton c 1

stringCounts :: String -> Counts
stringCounts s = mconcat $ fmap singleCounts s

instance Semigroup Counts where
    MkCounts p <> MkCounts q = MkCounts $ unionWith (+) p q

instance Monoid Counts where
    mempty = MkCounts mempty

type Entry = (Rule,Counts)

initialEntry :: Rule -> Entry
initialEntry r = (r,mempty)

type Table = [Entry]

entryCounts :: Entry -> Char -> Char -> Counts
entryCounts ((a1,b1,_),counts) a b = if a == a1 && b == b1 then counts else mempty

tableCounts :: Table -> Char -> Char -> Counts
tableCounts t a b = mconcat $ fmap (\e -> entryCounts e a b) t

doTableEntry :: Table -> Entry -> Entry
doTableEntry t (r@(a,b,c),_) = (r,tableCounts t a c <> tableCounts t c b <> singleCounts c)

doTable :: Table -> Table
doTable t = fmap (doTableEntry t) t

stringInsertedCounts :: Table -> String -> Counts
stringInsertedCounts t (a:b:ss) = tableCounts t a b <> stringInsertedCounts t (b:ss)
stringInsertedCounts _ _ = mempty

main :: IO ()
main = do
    inputstring <- readFile "app/2021/14/input.txt"
    let
        ll = lines inputstring
        startString = head ll
        rules :: [Rule]
        rules = mapMaybe parseRule ll

    do
        let
            t :: Transform
            t = mconcat $ fmap getTransform rules
            finalString :: String
            finalString = composeN 10 (doTransform t) startString
            countList :: [Int]
            countList = sort $ countElements finalString
        reportPart1 $ last countList - head countList
    do
        let
            table0 = fmap initialEntry rules
            tableN = composeN 40 doTable table0
            MkCounts counts = stringCounts startString <> stringInsertedCounts tableN startString
            countList :: [Int]
            countList = sort $ elems counts
        reportPart2 $ last countList - head countList
