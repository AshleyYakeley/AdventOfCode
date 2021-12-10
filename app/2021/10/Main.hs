{-# OPTIONS -Wno-incomplete-patterns #-}
module Main where
import Lib

errorscore :: String -> String -> Int
errorscore _ [] = 0
errorscore s (c:t) | elem c "([{<" = errorscore (c:s) t
errorscore ('(':s) (')':t) = errorscore s t
errorscore ('[':s) (']':t) = errorscore s t
errorscore ('<':s) ('>':t) = errorscore s t
errorscore ('{':s) ('}':t) = errorscore s t
errorscore _ (')':_) = 3
errorscore _ (']':_) = 57
errorscore _ ('}':_) = 1197
errorscore _ ('>':_) = 25137

completion :: String -> String -> Maybe String
completion s [] = Just s
completion s (c:t) | elem c "([{<" = completion (c:s) t
completion ('(':s) (')':t) = completion s t
completion ('[':s) (']':t) = completion s t
completion ('<':s) ('>':t) = completion s t
completion ('{':s) ('}':t) = completion s t
completion _ (')':_) = Nothing
completion _ (']':_) = Nothing
completion _ ('}':_) = Nothing
completion _ ('>':_) = Nothing

charScore :: Char -> Int
charScore '(' = 1
charScore '[' = 2
charScore '{' = 3
charScore '<' = 4

getScore :: Int -> String -> Int
getScore i [] = i
getScore i (c:cc) = getScore (i * 5 + charScore c) cc

getMiddle :: [a] -> a
getMiddle l = l !! (length l `div` 2)

main :: IO ()
main = do
    filestring <- readFile "app/2021/10/input.txt"
    let
        ll = lines filestring
    reportPart1 $ sum $ fmap (errorscore "") ll
    reportPart2 $ getMiddle $ sort $ fmap (getScore 0) $ mapMaybe (completion "") ll
