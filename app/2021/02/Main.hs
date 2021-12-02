module Main where
import System.IO
import Control.Monad.Trans.State
import Data.Foldable

parse :: [String] -> (Int,Int)
parse ["forward",s] = (read s,0)
parse ["up",s] = (0,negate $ read s)
parse ["down",s] = (0,read s)
parse x = error $ "unrecognised: " <> show x

move :: (Int,Int) -> State (Int,(Int,Int)) ()
move (f,aimd) = do
  (aim,(h,d)) <- get
  put (aim+aimd,(h + f,d + f * aim))

main :: IO ()
main = do
    f <- readFile "app/2021/02/input.txt"
    let pairs = fmap (parse . words) $ lines f
    putStrLn $ "Part 1: " <> show (sum (fmap fst pairs) * sum (fmap snd pairs))
    let (h,d) = snd $ snd $ runState (for_ pairs move) (0,(0,0))
    putStrLn $ "Part 2: " <> show (h * d)
