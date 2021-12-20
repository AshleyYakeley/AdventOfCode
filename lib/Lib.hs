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
import Text.Regex.TDFA as I hiding(empty)

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
