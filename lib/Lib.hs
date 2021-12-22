module Lib(module I,module Lib) where

import Data.Foldable as I
import Data.Traversable as I
import Data.Maybe as I
import Data.List as I (sort,sortBy,sortOn,reverse,nub)
import Data.Set as I (Set,fromList)
import Control.Applicative as I
import Data.Array.IArray as I hiding (range,elems,assocs)
import Data.Array.IArray (range)
import Data.Array.MArray as I hiding (range)
import Data.Array.Unboxed as I (UArray)
import Data.Array.IO as I hiding (range)
import Data.IORef as I
import Control.Monad.Trans.Class as I
import Control.Monad.Trans.State as I
import Control.Monad.Trans.Reader as I hiding (liftCatch,liftCallCC)
import Data.Monoid as I
import Data.Char as I
import Debug.Trace as I
import Data.Map as I  (Map,unionWith,singleton,elems,assocs)
import qualified Data.Map
import Text.Regex.TDFA as I hiding(empty)
import Data.Functor.Identity as I

reportPart1 :: Show a => a -> IO ()
reportPart1 a = putStrLn $ "Part 1: " <> show a

reportPart2 :: Show a => a -> IO ()
reportPart2 a = putStrLn $ "Part 2: " <> show a

-- copied from https://stackoverflow.com/a/4981265
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitString :: Char -> String -> [String]
splitString c = wordsWhen ((==) c)

flookup :: Eq a => a -> [(a, b)] -> b
flookup a ab = fromMaybe (error "Nothing") $ lookup a ab

headM :: [a] -> Maybe a
headM (a:_) = Just a
headM [] = Nothing

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith [] s = Just s
startsWith (a:aa) (b:bb) | a == b = startsWith aa bb
startsWith _ _ = Nothing

modifyList :: Int -> (a -> a) -> [a] -> [a]
modifyList i f aa = take i aa <> (f (aa !! i) : drop (succ i) aa)

composeN :: Int -> (a -> a) -> (a -> a)
composeN 0 _ = id
composeN n aa = aa . composeN (pred n) aa

while :: Monad m => m Bool -> m ()
while mm = do
    b <- mm
    if b then while mm else return ()

getMatches :: String -> String -> [String]
getMatches pat str = let
    (_ :: String,_ :: String,_ :: String,matches) = str =~ pat
    in matches

type Parser t = StateT [t] Maybe

readEnd :: Parser t ()
readEnd = StateT $ \case
    [] -> Just ((),[])
    (_:_) -> Nothing

readToken :: Parser t t
readToken = StateT $ \case
    [] -> Nothing
    (t:tt) -> Just (t,tt)

readThis :: Eq t => t -> Parser t ()
readThis t = do
    t' <- readToken
    if t == t' then return () else empty

runParser :: Parser t a -> [t] -> a
runParser p t = case runStateT p t of
    Nothing -> error "parse failed"
    Just (a,_) -> a

makeArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
makeArray bb f = array bb $ fmap (\i -> (i,f i)) $ range bb

newtype Hist a = MkHist (Map a Integer)

instance Ord a => Semigroup (Hist a) where
    MkHist m1 <> MkHist m2 = MkHist $ unionWith (+) m1 m2

instance Ord a => Monoid (Hist a) where
    mempty = MkHist mempty

histLookup :: Ord a => Hist a -> a -> Integer
histLookup (MkHist m) a = case Data.Map.lookup a m of
    Just i -> i
    Nothing -> 0

histAddAssoc :: Ord a => (a,Integer) -> Hist a -> Hist a
histAddAssoc (a,i) h@(MkHist m) = MkHist $ Data.Map.insert a (i + histLookup h a) m

histAdd :: Ord a => a -> Hist a -> Hist a
histAdd a = histAddAssoc (a,1)

histFromList :: Ord a => [a] -> Hist a
histFromList [] = mempty
histFromList (a:aa) = histAdd a $ histFromList aa

histFromAssocs :: Ord a => [(a,Integer)] -> Hist a
histFromAssocs [] = mempty
histFromAssocs (a:aa) = histAddAssoc a $ histFromAssocs aa

histAssocs :: Hist a -> [(a,Integer)]
histAssocs (MkHist m) = assocs m

--
newtype HistMonad a = MkHistMonad {unHistMonad :: [(a,Integer)]}

instance Functor HistMonad where
    fmap ab (MkHistMonad m) = MkHistMonad $ fmap (\(a,i) -> (ab a,i)) m

instance Applicative HistMonad where
    pure a = MkHistMonad $ pure (a,1)
    liftA2 f ma mb = ma >>= \a -> fmap (\b -> f a b) mb

instance Monad HistMonad where
    return = pure
    MkHistMonad m >>= f = MkHistMonad $ do
        (a,i) <- m
        (b,j) <- unHistMonad $ f a
        return (b, i * j)

hmReturnMany :: [a] -> HistMonad a
hmReturnMany aa = MkHistMonad $ fmap (\a -> (a,1)) aa

hmCollapse :: Ord a => HistMonad a -> HistMonad a
hmCollapse (MkHistMonad m) = MkHistMonad $ histAssocs $ histFromAssocs m

hmFilter :: (a -> Bool) -> HistMonad a -> HistMonad a
hmFilter f (MkHistMonad m) = MkHistMonad $ filter (\(a,_) -> f a) m

hmCount :: HistMonad a -> Integer
hmCount (MkHistMonad m) = sum $ fmap snd m
