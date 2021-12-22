{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Main(main) where
import Lib
import Control.Exception

data PState = MkPState {
    psScore :: Int,
    psPosition :: Int
} deriving (Eq,Ord)

newPState :: Int -> PState
newPState i = MkPState {psScore = 0,psPosition = i}

movePlayer :: Int -> Int -> State PState Bool
movePlayer maxscore i = do
    MkPState score pos <- get
    let
        newpos = succ $ mod (pred $ pos + i) 10
        newscore = score + newpos
    put $ MkPState newscore newpos
    return $ newscore >= maxscore

data Game m = MkGame {
    gMaxScore :: Int,
    gCollapse :: forall a. Ord a => m a -> m a,
    gRollDie :: m Int,
    gDoPlayer1 :: forall a. State PState a -> m a,
    gDoPlayer2 :: forall a. State PState a -> m a
}

move :: Monad m => Game m -> Bool -> m Bool
move MkGame{..} player = id $ do
    r <- gRollDie
    (if player then gDoPlayer2 else gDoPlayer1) $ movePlayer gMaxScore r

play :: Monad m => Game m -> m Bool
play game = do
    w <- move game False
    case w of
        True -> return False
        False -> do
            w2 <- move game True
            case w2 of
                True -> return True
                False -> play game

data GState die = MkGState {
    gsDie :: die,
    gsPlayer1 :: PState,
    gsPlayer2 :: PState
} deriving (Eq,Ord)

type S die = State (GState die)

type DState = Int
rollDeterminist :: S DState Int
rollDeterminist = do
    ss <- get
    let
        i = gsDie ss
    put $ ss {gsDie = succ i}
    return $ succ $ mod i 100

game1 :: Game (S DState)
game1 = MkGame {
    gMaxScore = 1000,
    gCollapse = id,
    gRollDie = do
        r1 <- rollDeterminist
        r2 <- rollDeterminist
        r3 <- rollDeterminist
        return $ r1 + r2 + r3,
    gDoPlayer1 = \(StateT m) -> StateT $ \olds -> do
        (a,news) <- m $ gsPlayer1 olds
        return (a,olds{gsPlayer1 = news}),
    gDoPlayer2 = \(StateT m) -> StateT $ \olds -> do
        (a,news) <- m $ gsPlayer2 olds
        return (a,olds{gsPlayer2 = news})
}

play1 :: S DState Int
play1 = do
    w <- play game1
    MkGState {..} <- get
    return $ gsDie * (psScore $ if w then gsPlayer1 else gsPlayer2)

rollDirac :: HistMonad Int
rollDirac = hmReturnMany [1,2,3]

game2 :: Game (StateT (PState,PState) HistMonad)
game2 = MkGame {
    gMaxScore = 21,
    gCollapse = id,
    gRollDie = lift $ hmCollapse $ do
        r1 <- rollDirac
        r2 <- rollDirac
        r3 <- rollDirac
        return $ r1 + r2 + r3,
    gDoPlayer1 = \(StateT m) -> StateT $ \(s1,s2) -> return $ runIdentity $ do
        (a,news) <- m s1
        return (a,(news,s2)),
    gDoPlayer2 = \(StateT m) -> StateT $ \(s1,s2) -> return $ runIdentity $ do
        (a,news) <- m s2
        return (a,(s1,news))
}

main :: IO ()
main = do
    inputstring <- readFile "app/2021/21/input.txt"
    let
        ll = lines inputstring
        sp1 = newPState $ read $ last $ splitString ' ' $ ll !! 0
        sp2 = newPState $ read $ last $ splitString ' ' $ ll !! 1
    reportPart1 $ runIdentity $ evalStateT play1 $ MkGState 0 sp1 sp2
    let
        h = evalStateT (play game2) (sp1,sp2)
    _ <- evaluate $ hmCount h
    let
        p1wins = hmCount $ hmFilter not h
        p2wins = hmCount $ hmFilter id h
    reportPart2 $ max p1wins p2wins
