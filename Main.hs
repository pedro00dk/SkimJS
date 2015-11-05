import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    e <- evalExpr env expr
    case v of
        Gvar -> createGlobalVar var e
        _ -> setVar var e
---------------------------------------------------------------------------------------------------
-- Call Function -- CallExpr Expression [Expression] -- ^ @f(x,y,z)@, spec 11.2.3
evalExpr env (CallExpr name params) = do
    f <- evalExpr env name
    case f of
        Function _ args stmts -> do
            pushScope env
            createArgs env args params
            evalStmt env (BlockStmt stmts)
            popScope env
        _ -> error $ "Variable " ++ show name ++ " not is a function"

---------------------------------------------------------------------------------------------------
-- Create the local parameter variables
createArgs :: StateT -> [Id] -> [Expression] -> StateTransformer Value
createArgs env [] [] = return Nil
createArgs env ((Id arg):xs) (param:ys) = do
    v <- evalExpr env param
    createLocalVar arg v
    createArgs env xs ys
createArgs env _ _ = error $ "Invalid amount of parameters"
---------------------------------------------------------------------------------------------------


evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
---------------------------------------------------------------------------------------------------
--Block
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt:stmts)) = do
    v <- evalStmt env stmt
    case v of
        Break -> return Break
        Continue -> return Continue
        Throw t -> return (Throw t)
        _ -> evalStmt env (BlockStmt stmts)

-- If
evalStmt env (IfSingleStmt expr stmt) = do
    Bool v <- evalExpr env expr
    if v then do
        pushScope env
        a <- evalStmt env stmt
        popScope env
        return a
    else return Nil

--If Else
evalStmt env (IfStmt expr stmt1 stmt2) = do
    Bool v <- evalExpr env expr
    if v then do
        pushScope env
        a <- evalStmt env stmt1
        popScope env
        return a
    else do
        pushScope env
        a <- evalStmt env stmt2
        popScope env
        return a

-- While
evalStmt env (WhileStmt expr stmt) = do
    Bool b <- evalExpr env expr
    if b then do 
        pushScope env
        v <- evalStmt env stmt
        popScope env
        case v of
            Break -> return Break
            Throw t -> return (Throw t)
            _ -> evalStmt env (WhileStmt expr stmt)
        
    else return Nil

-- Do While
evalStmt env (DoWhileStmt stmt expr) = do
    pushScope env
    v <- evalStmt env stmt
    case v of
        Break -> do
            popScope env
            return Nil
        Throw t -> do
            popScope env
            return (Throw t)
        _ -> do
            evalStmt env (WhileStmt expr stmt)
            popScope env
-- Break
evalStmt env (BreakStmt m) = return Break;

-- Continue
evalStmt env (ContinueStmt m) = return Continue;

-- Throw
evalStmt env (ThrowStmt expr) = do
    v <- evalExpr env expr
    return (Throw v)

-- functions
-- saving a function as a value
evalStmt env (FunctionStmt (Id name) args stmts) = createGlobalVar name (Function (Id name) args stmts)

---------------------------------------------------------------------------------------------------

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: [Map String Value]
environment = [empty]

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    case scopeLookup s var of
        Nothing -> (Gvar, s)
        Just val -> (val, s)

scopeLookup :: [Map String Value] -> String -> Maybe Value
scopeLookup [] _ = Nothing
scopeLookup (s:scopes) var =
    case Map.lookup var s of
        Nothing -> scopeLookup scopes var
        Just val -> Just val

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> createLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            createLocalVar id val

-- Modified to set a var in the top scope
setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, (updateVar var val s))
    
updateVar :: String -> Value -> StateT -> StateT
updateVar _ _ [] = error $ "erro"
updateVar var val stt = case (Map.lookup var (head stt)) of
        Nothing -> (head stt):(updateVar var val (tail stt))
        Just v -> (insert var val (head stt)):(tail stt)

createLocalVar :: String -> Value -> StateTransformer Value
createLocalVar var val = ST $ \s -> (val, (insert var val (head s)):(tail s))

createGlobalVar :: String -> Value -> StateTransformer Value
createGlobalVar var val = ST $ \s -> (val, createGlobalVar0 var val s)
 
createGlobalVar0 :: String -> Value -> StateT -> StateT
createGlobalVar0 var val stt = if null (tail stt) then (insert var val (head stt)):[] else (head stt):(createGlobalVar0 var val (tail stt))

-- Function to create a new scope
pushScope :: StateT -> StateTransformer Value
pushScope env = ST $ \s -> (Nil, empty:s)

-- Function to delete the top scope
popScope :: StateT -> StateTransformer Value
popScope env = ST $ \s -> (Nil, (tail s))


--
-- Types and boilerplate
--

type StateT = [Map String Value]

data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, []) = ""
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union (head defs) (head environment)) ++ "\n" ++ showResult (val, tail(defs))

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
