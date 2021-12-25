{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
module Main(main) where
import Lib

data SCT = SCTRight | SCTDown

type Point = (Int,Int)
type Board = ((Int,Int),Array Point (Maybe SCT))

getSCT :: Char -> Maybe SCT
getSCT '.' = Nothing
getSCT '>' = Just SCTRight
getSCT 'v' = Just SCTDown

readBoard :: [String] -> Board
readBoard ss =
    let
        rlen = length ss
        clen = length $ head ss
        bd = listArray ((0,0),(pred rlen, pred clen)) $ fmap getSCT $ mconcat ss
    in ((rlen,clen),bd)

type M = StateT Board IO

replaceArray :: ((Int,Int) -> (Point -> Maybe SCT) -> Point -> IO (Maybe SCT)) -> M ()
replaceArray f = do
    ((rlen,clen),board) <- get
    board' <- liftIO $ makeArrayM ((0,0),(pred rlen, pred clen)) $ \rc -> f (rlen,clen) (\i -> board ! i) rc
    put ((rlen,clen),board')

moveRight :: M Bool
moveRight = do
    flag <- liftIO $ newIORef False
    replaceArray $ \(_rlen,clen) oldboard (r,c) -> do
        let
            msct0 = oldboard (r,mod (pred $ clen + c) clen)
            msct1 = oldboard (r,c)
            msct2 = oldboard (r,mod (succ c) clen)
        case (msct0,msct1,msct2) of
            (Just SCTRight,Nothing,_) -> do
                writeIORef flag True
                return $ Just SCTRight
            (_,Just SCTRight,Nothing) -> do
                writeIORef flag True
                return Nothing
            _ -> return msct1
    liftIO $ readIORef flag

moveDown :: M Bool
moveDown = do
    flag <- liftIO $ newIORef False
    replaceArray $ \(rlen,_clen) oldboard (r,c) -> do
        let
            msct0 = oldboard (mod (pred $ rlen + r) rlen,c)
            msct1 = oldboard (r,c)
            msct2 = oldboard (mod (succ r) rlen,c)
        case (msct0,msct1,msct2) of
            (Just SCTDown,Nothing,_) -> do
                writeIORef flag True
                return $ Just SCTDown
            (_,Just SCTDown,Nothing) -> do
                writeIORef flag True
                return Nothing
            _ -> return msct1
    liftIO $ readIORef flag

moves :: M Int
moves = do
    b1 <- moveRight
    b2 <- moveDown
    if b1 || b2
        then fmap succ $ moves
        else return 1

main :: IO ()
main = do
    inputstring <- readFile "app/2021/25/input.txt"
    let
        board = readBoard $ lines inputstring
    i <- evalStateT moves board
    reportPart1 $ i
