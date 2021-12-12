{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main where
import Lib

type Cave = String

caveSmall :: Cave -> Bool
caveSmall (c:_) = isLower c

type System = [(Cave,Cave)]

readCLine :: String -> (Cave,Cave)
readCLine s = let
    [a,b] = wordsWhen ((==) '-') s
    in (a,b)

type M1 = ReaderT [Cave] []
type M2 = ReaderT (Bool,[Cave]) []

main :: IO ()
main = do
    inputstring <- readFile "app/2021/12/input.txt"
    let
        system :: System
        system = fmap readCLine $ lines inputstring

        caveChoices :: Cave -> [Cave]
        caveChoices c = mapMaybe (\(a,b) -> if a == c then Just b else if b == c then Just a else Nothing) system

    do
        let
            step :: Cave -> M1 ()
            step c = do
                c' <- lift $ caveChoices c
                check c'

            check :: Cave -> M1 ()
            check "start" = lift []
            check "end" = return ()
            check c | caveSmall c = do
                cc <- ask
                if elem c cc
                    then lift []
                    else local (\_ -> c:cc) $ step c
            check c = step c

        reportPart1 $ length $ runReaderT (step "start") []

    do
        let
            step :: Cave -> M2 ()
            step c = do
                c' <- lift $ caveChoices c
                check c'

            check :: Cave -> M2 ()
            check "start" = lift []
            check "end" = return ()
            check c | caveSmall c = do
                (i,cc) <- ask
                case (i,length $ filter (== c) cc) of
                    (_,0) -> local (\_ -> (i,c:cc)) $ step c
                    (False,1) ->  local (\_ -> (True,c:cc)) $ step c
                    _ -> lift []
            check c = step c

        reportPart2 $ length $ runReaderT (step "start") (False,[])
