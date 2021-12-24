{-# OPTIONS -Wno-incomplete-patterns #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module Main(main) where
import Lib hiding (rangeSize)

data Func = Add | Mul | Div | Mod deriving (Eq)

instance Show Func where
    show Add = "+"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

evalFunc :: Func -> Integer -> Integer -> Integer
evalFunc Add a b = a + b
evalFunc Mul a b = a * b
evalFunc Div a b = div a b
evalFunc Mod a b = mod a b

data I = ConstI Integer | InputI Int | VarI Int I | FuncI Func I I | ITEI I I I I deriving (Eq)

pattern AddI :: I -> I -> I
pattern AddI a b = FuncI  Add a b

pattern MulI :: I -> I -> I
pattern MulI a b = FuncI  Mul a b

pattern DivI :: I -> I -> I
pattern DivI a b = FuncI  Div a b

pattern ModI :: I -> I -> I
pattern ModI a b = FuncI  Mod a b

instance Show I where
    show (ConstI v) = show v
    show (InputI n) = "I" <> show n
    show (FuncI f a b) = "(" <> show a <> show f <> show b <> ")"
    show (VarI n _) = "V" <> show n
    show (ITEI t1 t2 a b) = "(if " <> show t1 <> "=" <> show t2 <> " then " <> show a <> " else " <> show b <> ")"

funcI :: Func -> I -> I -> I
funcI Add a (ConstI 0) = a
funcI Add (ConstI 0) b = b
funcI Add (AddI a b) c = funcI Add a (funcI Add b c)
funcI Mul (ConstI 0) _ = ConstI 0
funcI Mul _ (ConstI 0) = ConstI 0
funcI Mul (ConstI 1) b = b
funcI Mul a (ConstI 1) = a
funcI Mul (MulI a b) c = funcI Mul a (funcI Mul b c)
funcI Div a (ConstI 1) = a
funcI Mod (MulI _ b) d | b == d = ConstI 0
funcI Mod (AddI a@(MulI _ b) c) d | b == d = funcI Add (funcI Mod a d) (funcI Mod c d)
funcI Div (AddI a b) c = funcI Add (funcI Div a c) (funcI Div b c)
funcI Div (MulI a b) c | b == c = a
funcI Div (DivI a b) c = funcI Div a $ funcI Mul b c
-- funcI Mod (VarI _ a) b@(ConstI _) = funcI Mod a b
-- funcI Div (VarI _ a) b@(ConstI _) = funcI Div a b
funcI f (ConstI a) (ConstI b) = ConstI $ evalFunc f a b
funcI f a@(ConstI _) (ITEI ta tb p q) = iteiI ta tb (funcI f a p) (funcI f a q)
funcI f a@(FuncI _ _ _) (ITEI ta tb p@(ConstI _) q@(ConstI _)) = iteiI ta tb (funcI f a p) (funcI f a q)
funcI f a@(InputI _) (ITEI ta tb p@(ConstI _) q@(ConstI _)) = iteiI ta tb (funcI f a p) (funcI f a q)
funcI f a@(VarI _ _) (ITEI ta tb p@(ConstI _) q@(ConstI _)) = iteiI ta tb (funcI f a p) (funcI f a q)
funcI f (ITEI ta tb p q) a@(ConstI _) = iteiI ta tb (funcI f p a) (funcI f q a)
funcI f (ITEI ta tb p@(ConstI _) q@(ConstI _)) a@(FuncI _ _ _) = iteiI ta tb (funcI f p a) (funcI f q a)
funcI f (ITEI ta tb p@(ConstI _) q@(ConstI _)) a@(InputI _) = iteiI ta tb (funcI f p a) (funcI f q a)
funcI f (ITEI ta tb p@(ConstI _) q@(ConstI _)) a@(VarI _ _) = iteiI ta tb (funcI f p a) (funcI f q a)
funcI f (ITEI ta1 tb1 p1 q1) (ITEI ta2 tb2 p2 q2) | ta1 == ta2, tb1 == tb2 = iteiI ta1 tb1 (funcI f p1 p2) (funcI f q1 q2)
funcI f a b | Just (p,q) <- boundsI (FuncI f a b), p == q = ConstI p
funcI f a b = FuncI f a b

boundsI :: I -> Maybe (Integer,Integer)
boundsI (VarI _ v) = boundsI v
boundsI (ConstI i) = Just (i,i)
boundsI (InputI _) = Just (1,9)
boundsI (AddI p q) = do
    (pa,pb) <- boundsI p
    (qa,qb) <- boundsI q
    return (pa + qa,pb + qb)
boundsI (MulI p q) = do
    (pa,pb) <- boundsI p
    (qa,qb) <- boundsI q
    if pa >= 0 && qa >= 0 then return (pa * qa,pb * qb) else Nothing
boundsI (ModI p (ConstI i)) | i >= 0 = do
    (a,_) <- boundsI p
    if a >= 0 then return (0,pred i) else Nothing
boundsI (DivI p (ConstI i)) | i >= 0 = do
    (a,b) <- boundsI p
    if a >= 0 then return (div a i,div b i) else Nothing
boundsI (ITEI _ _ p q) = do
    (pa,pb) <- boundsI p
    (qa,qb) <- boundsI q
    return (min pa qa,max pb qb)
boundsI _ = Nothing

nonOverlap :: (Integer, Integer) -> (Integer, Integer) -> Bool
nonOverlap (a1,b1) (a2,b2) = a2 > b1 || a1 > b2

iteiI :: I -> I -> I -> I -> I
iteiI ta tb _ b | Just bta <- boundsI ta, Just btb <- boundsI tb, nonOverlap bta btb = b
iteiI ta tb a _ | ta == tb = a
iteiI ta tb a b = ITEI ta tb a b

equalsI :: I -> I -> I
equalsI (ITEI ta tb a b) (ConstI 0) = iteiI ta tb b a
equalsI ta tb = iteiI ta tb (ConstI 1) (ConstI 0)

containsVar :: Int -> I -> Bool
containsVar n (VarI n' _) = n == n'
containsVar n (FuncI _ a b) = containsVar n a || containsVar n b
containsVar n (ITEI ta tb a b) = containsVar n ta || containsVar n tb || containsVar n a || containsVar n b
containsVar _ _ = False

type Registers = [I]

isVar :: Char -> Maybe Int
isVar 'w' = Just 0
isVar 'x' = Just 1
isVar 'y' = Just 2
isVar 'z' = Just 3
isVar _ = Nothing

regVar :: Int -> (Registers -> I, I -> Registers -> Registers)
regVar n = (\r -> r !! n,\v r -> take n r <> [v] <> drop (succ n) r )

data MState = MkMState {
    msRegisters :: Registers,
    msInput :: [I],
    msLocalVarName :: Int,
    msLocalVars :: [(Int,I,Maybe (Integer,Integer))]
}

type M = StateT MState IO

input :: M I
input = do
    ms <- get
    case msInput ms of
        (i:ii) -> do
            put $ ms{msInput = ii}
            return i
        [] -> liftIO $ fail "empty state"

getField :: String -> M I
getField [c] | Just n <- isVar c =  do
    ms <- get
    return $ fst (regVar n) $ msRegisters ms
getField s = return $ ConstI $ read s

putField :: String -> I -> M ()
putField [c] v = do
    let Just n = isVar c
    ms <- get
    put $ ms{msRegisters = snd (regVar n) v $ msRegisters ms}

replaceRegister :: String -> M ()
replaceRegister reg = do
    expr <- getField reg
    case expr of
        ConstI _ -> return ()
        InputI _ -> return ()
        VarI _ _ -> return ()
        -- e | ifless e -> return ()
        _ -> do
            ms <- get
            let
                var = msLocalVarName ms
                vari = VarI var expr
            put $ ms {msLocalVarName = succ var,msLocalVars = msLocalVars ms <> [(var,expr,boundsI expr)]}
            putField reg vari

replaceRegisters :: M ()
replaceRegisters = do
    replaceRegister "w"
    replaceRegister "x"
    replaceRegister "y"
    replaceRegister "z"

getInstruction :: String -> M ()
getInstruction s = let
    args = splitString ' ' s
    arg1 = args !! 1
    arg2 = args !! 2
    in case args !! 0 of
        "vars" -> replaceRegisters
        "var" -> replaceRegister arg1
        "inp" -> do
            replaceRegisters
            val <- input
            putField arg1 val
        "add" -> do
            val1 <- getField arg1
            val2 <- getField arg2
            putField arg1 $ funcI Add val1 val2
        "mul" -> do
            val1 <- getField arg1
            val2 <- getField arg2
            putField arg1 $ funcI Mul val1 val2
        "div" -> do
            val1 <- getField arg1
            val2 <- getField arg2
            putField arg1 $ funcI Div val1 val2
        "mod" -> do
            val1 <- getField arg1
            val2 <- getField arg2
            putField arg1 $ funcI Mod val1 val2
        "eql" -> do
            val1 <- getField arg1
            val2 <- getField arg2
            putField arg1 $ equalsI val1 val2

runAll :: [M ()] -> M ()
runAll [] = return ()
runAll (m:mm) = m >> runAll mm


pattern Mul26I :: I -> I
pattern Mul26I a = MulI a (ConstI 26)

pattern Div26I :: I -> I
pattern Div26I a = DivI a (ConstI 26)

pattern Mod26I :: I -> I
pattern Mod26I a = ModI a (ConstI 26)


data PushPop = Push Int Integer | Pop Int Integer

instance Show PushPop where
    show (Push i c) = "push I" <> show i <> " + " <> show c
    show (Pop i c) = "I" <> show i <> " = pop + " <> show c

toPushPop :: I -> PushPop
toPushPop (AddI (InputI i) (ConstI c)) = Push i c
toPushPop (AddI (Mul26I (VarI _ _)) (AddI (InputI i) (ConstI c))) = Push i c
toPushPop (ITEI (AddI (Mod26I (VarI p1 _)) (ConstI a)) (InputI i) (Div26I (VarI p2 _)) (AddI (Mul26I (Div26I (VarI p3 _))) _)) | p1 == p2,p1 == p3 = Pop i a

runM :: M () -> IO [PushPop]
runM m = do
    ms <- let
        msRegisters = [ConstI 0,ConstI 0,ConstI 0,ConstI 0]
        msInput = fmap InputI [0..13]
        msLocalVarName = 0
        msLocalVars = []
        in execStateT m $ MkMState {..}
    let
        result = msRegisters ms !! 3
        vars = pruneLocalVars result $ msLocalVars ms
        ppp = fmap (\(_,expr,_) -> toPushPop expr) vars
    return ppp

pruneLocalVars :: I -> [(Int, I, a)] -> [(Int, I, a)]
pruneLocalVars _ [] = []
pruneLocalVars expres (v@(n,_,_):vv) = if any (containsVar n) $ expres : fmap (\(_,e,_) -> e) vv then v : pruneLocalVars expres vv else pruneLocalVars expres vv

type Stack = [(Int,Integer)]

type StackM = StateT Stack IO

type Eqn = (Int,Int,Integer)

doStack :: PushPop -> StackM [Eqn]
doStack (Push i c) = do
    modify (\ss -> (i,c):ss)
    return []
doStack (Pop i c) = do
    (i',c'):ss <- get
    put ss
    return [(i,i',c' + c)]

minEqn :: Eqn -> [(Int,Integer)]
minEqn (i,i',d) = [(i,max 1 (1 + d)),(i',max 1 (1 - d))]

maxEqn :: Eqn -> [(Int,Integer)]
maxEqn (i,i',d) = [(i,min 9 (9 + d)),(i',min 9 (9 - d))]

main :: IO ()
main = do
    inputstring <- readFile "app/2021/24/input.txt"
    let
        ll = lines inputstring
        instructions = runAll $ (fmap getInstruction ll) <> [replaceRegister "z"]
    ppp <- runM instructions
    eqnss <- evalStateT (for ppp doStack) []
    let eqns = mconcat eqnss
    reportPart1 $ mconcat $ fmap (show . snd) $ sortOn fst $ mconcat $ fmap maxEqn eqns
    reportPart2 $ mconcat $ fmap (show . snd) $ sortOn fst $ mconcat $ fmap minEqn eqns
