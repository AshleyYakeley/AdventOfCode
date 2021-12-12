module Lib(module I,module Lib) where

import Data.Foldable as I
import Data.Traversable as I
import Data.Maybe as I
import Data.List as I (sort,sortBy,sortOn,reverse,nub)
import Data.Set as I (Set,fromList)
import Control.Applicative as I
import Data.Array.MArray as I hiding (range)
import Data.Array.IO as I hiding (range)
import Data.IORef as I
import Control.Monad.Trans.Class as I
import Control.Monad.Trans.State as I
import Control.Monad.Trans.Reader as I hiding (liftCatch,liftCallCC)
import Data.Monoid as I
import Data.Char as I
import Debug.Trace as I

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

flookup :: Eq a => a -> [(a, b)] -> b
flookup a ab = fromMaybe (error "Nothing") $ lookup a ab

headM :: [a] -> Maybe a
headM (a:_) = Just a
headM [] = Nothing
