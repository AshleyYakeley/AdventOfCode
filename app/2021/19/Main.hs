{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

type Pos = (Int,Int,Int)

type Scan = [Pos]

type P = Parser String

readPos :: P Pos
readPos = do
    s <- readToken
    case splitString ',' s of
        [x,y,z] -> return (read x,read y,read z)
        _ -> empty

readScan :: P Scan
readScan = do
    s <- readToken
    case s of
        ('-':'-':_) -> return ()
        _ -> empty
    scan <- many readPos
    _ <- many $ readThis ""
    return scan

readScans :: P [Scan]
readScans = do
    ss <- many readScan
    readEnd
    return ss

-- https://gist.github.com/sigman78/8010812
rotations :: [Pos -> Pos]
rotations = let
    a :: Pos -> Pos
    a (x,y,z) = (x,z,-y)

    b :: Pos -> Pos
    b (x,y,z) = (-z,y,x)

    a2 = a . a
    a3 = a . a2
    b2 = b . b
    b3 = b . b2
    in [ x . y | x <- [id, a, a2, a3], y <- [id, b, b2, b3, b . a, b . a3]]

newtype SortedScan = MkSortedScan {unSortedScan :: Scan}

mkSortedScan :: Scan -> SortedScan
mkSortedScan s = MkSortedScan $ sort s

joinScans :: SortedScan -> SortedScan -> SortedScan
joinScans (MkSortedScan sa) (MkSortedScan sb) = let
    j aa [] = aa
    j [] bb = bb
    j (a:aa) (b:bb) = case compare a b of
        EQ -> a : j aa bb
        LT -> a : j aa (b:bb)
        GT -> b : j (a:aa) bb
    in MkSortedScan $ j sa sb

scanMatch :: SortedScan -> SortedScan -> Int
scanMatch (MkSortedScan sa) (MkSortedScan sb) = let
    sm _ [] = 0
    sm [] _ = 0
    sm (a:aa) (b:bb) = case compare a b of
        EQ -> succ $ sm aa bb
        LT -> sm aa (b:bb)
        GT -> sm (a:aa) bb
    in sm sa sb

diffPos :: Pos -> Pos -> Pos
diffPos (x,y,z) (x',y',z') = (x' - x, y' - y, z' -z)

moveScan :: Pos -> SortedScan -> SortedScan
moveScan p (MkSortedScan s) = MkSortedScan $ fmap (diffPos p) s

rotatedScans :: Scan -> [SortedScan]
rotatedScans s = fmap (\r -> mkSortedScan $ fmap r s) rotations

matchTranslatedScan :: SortedScan -> SortedScan -> Maybe (SortedScan,Pos)
matchTranslatedScan s0 s = headM $ filter (\(s',_) -> scanMatch s0 s' >= 12) $ fmap (\d -> (moveScan d s,d)) $ [diffPos p0 p|p0 <- drop 11 (unSortedScan s0), p <- unSortedScan s]

matchScan :: SortedScan -> Scan -> Maybe (SortedScan,Pos)
matchScan s0 s = headM $ mapMaybe (matchTranslatedScan s0) $ rotatedScans s

adduceScans :: SortedScan -> [Scan] -> (SortedScan,[Scan],[Pos])
adduceScans s0 [] = (s0,[],[])
adduceScans s0 (s:ss) = case matchScan s0 s of
    Just (s',p) -> let
        (s0',ss',pp) = adduceScans (joinScans s0 s') ss
        in (s0',ss',p:pp)
    Nothing -> let
        (s0',ss',pp) = adduceScans s0 ss
        in (s0',s:ss',pp)

runScans :: SortedScan -> [Scan] -> (SortedScan,[Scan],[Pos])
runScans s0 ss = let
    r@(s0',ss',pp') = adduceScans s0 ss
    in if length ss' < length ss then let (s0'',ss'',pp'') = runScans s0' ss' in (s0'',ss'',pp' <> pp'') else r

manhattan :: Pos -> Pos -> Int
manhattan (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

main :: IO ()
main = do
    inputstring <- readFile "app/2021/19/input.txt"
    let
        ll = lines inputstring
        s0:ss = runParser readScans ll
        (MkSortedScan scan,[],pp) = runScans (mkSortedScan s0) ss
        scanpos = (0,0,0):pp

    reportPart1 $ length scan
    reportPart2 $ maximum $ [manhattan p1 p2| p1 <- scanpos, p2 <- scanpos]
