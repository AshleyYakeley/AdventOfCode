module Main where
import Control.Monad.Trans.State
import Data.Maybe

-- copied from https://stackoverflow.com/a/4981265
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

type Board = [[Maybe Int]]

getBLine :: String -> [Maybe Int]
getBLine s = fmap (Just . read) $ words s

getBoards :: [String] -> [Board]
getBoards [] = []
getBoards ("":a:b:c:d:e:rest) = fmap getBLine [a,b,c,d,e] : getBoards rest

boardSum :: Board -> Int
boardSum b = sum $ fmap (\l -> sum $ fmap (fromMaybe 0) l) b

winRow :: Board -> Bool
winRow = any (all (not . isJust))

winCol :: Board -> Bool
winCol b = any (\i -> all (\l -> not $ isJust $ l !! i) b) [0..4]

winBoard :: Board -> Bool
winBoard b = winRow b || winCol b

type M = State [Board]

boardMove :: Int -> Board -> Board
boardMove i = fmap $ fmap $ \mx -> if mx == Just i then Nothing else mx

getNewBoards :: Int -> M [Board]
getNewBoards i = do
    boards <- get
    return $ fmap (boardMove i) boards

part1 :: [Int] -> M Int
part1 [] = return $ error "boards left over"
part1 (i:ii) = do
    boards <- getNewBoards i
    let
        winBoards = filter winBoard boards
    put boards
    case winBoards of
        [b] -> return $ i * boardSum b
        [] -> part1 ii
        _ -> return $ error "multiple wins"

part2 :: [Int] -> M Int
part2 [] = return $ error "boards left over"
part2 (i:ii) = do
    boards <- getNewBoards i
    let
        winBoards = filter winBoard boards
        loseBoards = filter (not . winBoard) boards
    put loseBoards
    case (loseBoards,winBoards) of
        ([],[b]) -> return $ i * boardSum b
        ([],[]) -> return $ error "no wins"
        _ -> part2 ii

main :: IO ()
main = do
    f <- readFile "app/2021/04/input.txt"
    let
        numline : boardlines = lines f
        nums :: [Int]
        nums = fmap read $ wordsWhen ((==) ',') numline
        boards :: [Board]
        boards = getBoards boardlines
    putStrLn $ "Part 1: " <> (show $ evalState (part1 nums) boards)
    putStrLn $ "Part 2: " <> (show $ evalState (part2 nums) boards)
