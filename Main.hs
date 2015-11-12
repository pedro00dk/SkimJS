import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value
import Data.Bits

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
---------------------------------------------------------------------------------------------------
evalExpr env (StringLit str) = return $ String str
evalExpr env (ArrayLit []) = return $ Array []
evalExpr env (ArrayLit exprs) = parseArray env exprs (Array [])
evalExpr env (ObjectLit []) = return $ Object $ []
evalExpr env (ObjectLit props) = parseObject env props (Object [])

-- Accessing object properties using dot -> this just cant access Integer properties
evalExpr env (DotRef expr id) = do
    obj <- evalExpr env expr
    getObjectDotProperty env obj id

-- Accessing properties using brackets (from arrays or objects) -> can get all types of properties if is a object
evalExpr env (BracketRef expr idexpr) = do
    obj <- evalExpr env expr
    case obj of
        Array _ -> do
            id <- evalExpr env idexpr
            getArrayIndex env obj id
        Object _ -> do
            id <- evalExpr env idexpr
            getObjectBracketProperty env obj id
        _ -> error $ "Illegal type"
            

-- Unary assign expressions to object property values using dot
evalExpr env (UnaryAssignExpr op (LDot objexpr prop)) = do
    obj <- evalExpr env objexpr
    case op of
        PrefixInc -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpAdd act (Int 1)
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> do
                    setVar var obj -- updates the memory just if is a object var
                    return x
                _ -> return x
        PrefixDec -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpSub act (Int 1)
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> do
                    setVar var obj -- updates the memory just if is a object var
                    return x
                _ -> return x
        PostfixInc -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpAdd act (Int 1)
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> do
                    setVar var obj -- updates the memory just if is a object var
                    return act
                _ -> return act
        PostfixDec -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpAdd act (Int 1)
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> do
                    setVar var obj -- updates the memory just if is a object var
                    return act
                _ -> return act

-- Assign expressions to object property values using dot
evalExpr env (AssignExpr op (LDot objexpr prop) assignexpr) = do
    obj <- evalExpr env objexpr
    ass <- evalExpr env assignexpr
    case op of
        OpAssign -> do -- creates the property if not exists
            obj <- setObjectDotProperty env obj (Id prop) ass
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignAdd -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpAdd act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignSub -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpSub act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignMul -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpMul act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignDiv -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpDiv act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignMod -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpMod act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignLShift -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpLShift act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignSpRShift -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpSpRShift act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignZfRShift -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpZfRShift act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignBAnd -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpBAnd act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignBOr -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpBOr act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil
        OpAssignBXor -> do
            act <- getObjectDotProperty env obj (Id prop)
            x <- infixOp env OpBXor act ass
            obj <- setObjectDotProperty env obj (Id prop) x
            case objexpr of
                VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                _ -> return Nil

-- Unary assign expressions to object or arrays property values using brackets
evalExpr env (UnaryAssignExpr op (LBracket objexpr propexpr)) = do
    obj <- evalExpr env objexpr
    prop <- evalExpr env propexpr
    case obj of
        Array _ -> case op of
            PrefixInc -> do -- creates the property if not exists
                act <- getArrayIndex env obj prop
                x <- infixOp env OpAdd act (Int 1)
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return x
                    _ -> return x
            PrefixDec -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpSub act (Int 1)
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return x
                    _ -> return x
            PostfixInc -> do -- creates the property if not exists
                act <- getArrayIndex env obj prop
                x <- infixOp env OpAdd act (Int 1)
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return act
                    _ -> return act
            PostfixDec -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpSub act (Int 1)
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return act
                    _ -> return act
        Object _ -> case op of
            PrefixInc -> do -- creates the property if not exists
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpAdd act (Int 1)
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return x
                    _ -> return x
            PrefixDec -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpSub act (Int 1)
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return x
                    _ -> return x
            PostfixInc -> do -- creates the property if not exists
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpAdd act (Int 1)
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return act
                    _ -> return act
            PostfixDec -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpSub act (Int 1)
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> do
                        setVar var obj -- updates the memory just if is a object var
                        return act
                    _ -> return act
        _ -> error $ "Illegal access"


-- Assign expressions to object property values using brackets
evalExpr env (AssignExpr op (LBracket objexpr propexpr) assignexpr) = do
    obj <- evalExpr env objexpr
    ass <- evalExpr env assignexpr
    prop <- evalExpr env propexpr
    case obj of
        Array _ -> case op of
            OpAssign -> do -- creates the property if not exists
                obj <- setArrayIndex env obj prop ass
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignAdd -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpAdd act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignSub -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpSub act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignMul -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpMul act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignDiv -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpDiv act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignMod -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpMod act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignLShift -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpLShift act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignSpRShift -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpSpRShift act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignZfRShift -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpZfRShift act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBAnd -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpBAnd act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBOr -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpBOr act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBXor -> do
                act <- getArrayIndex env obj prop
                x <- infixOp env OpBXor act ass
                obj <- setArrayIndex env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
        Object _ -> case op of
            OpAssign -> do -- creates the property if not exists
                obj <- setObjectBracketProperty env obj prop ass
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignAdd -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpAdd act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignSub -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpSub act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignMul -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpMul act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignDiv -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpDiv act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignMod -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpMod act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignLShift -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpLShift act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignSpRShift -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpSpRShift act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignZfRShift -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpZfRShift act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBAnd -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpBAnd act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBOr -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpBOr act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
            OpAssignBXor -> do
                act <- getObjectBracketProperty env obj prop
                x <- infixOp env OpBXor act ass
                obj <- setObjectBracketProperty env obj prop x
                case objexpr of
                    VarRef (Id var) -> setVar var obj -- updates the memory just if is a object var
                    _ -> return Nil
        _ -> error $ "Illegal access"
---------------------------------------------------------------------------------------------------
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
---------------------------------------------------------------------------------------------------
-- Evaluation of expressions with prefixed operators
evalExpr env (PrefixExpr op expr) = do
    v <- evalExpr env expr
    prefixOp env op v

-- Unary assign -> i++, i--, ++i, --i
evalExpr env (UnaryAssignExpr op (LVar var)) = do
    Int v <- stateLookup env var
    case op of
        PrefixInc -> do
            setVar var (Int (v + 1))
            return (Int (v + 1))
        PrefixDec -> do
            setVar var (Int (v - 1))
            return (Int (v - 1))
        PostfixInc -> do
            setVar var (Int (v + 1))
            return $ Int v
        PostfixDec -> do
            setVar var (Int (v - 1))
            return $ Int v

-- Assign expression - Just simple assigns (OpAssign) creates automatic global variables
evalExpr env (AssignExpr op (LVar var) expr) = do
    v <- stateLookup env var
    e <- evalExpr env expr
    case op of
        OpAssign -> do
            case v of
                Gvar -> createGlobalVar var e
                _ -> setVar var e
        OpAssignAdd -> do
            x <- infixOp env OpAdd v e
            setVar var x
        OpAssignSub -> do
            x <- infixOp env OpSub v e
            setVar var x
        OpAssignMul -> do
            x <- infixOp env OpMul v e
            setVar var x
        OpAssignDiv -> do
            x <- infixOp env OpDiv v e
            setVar var x
        OpAssignMod -> do
            x <- infixOp env OpMod v e
            setVar var x
        OpAssignLShift -> do
            x <- infixOp env OpLShift v e
            setVar var x
        OpAssignSpRShift -> do
            x <- infixOp env OpSpRShift v e
            setVar var x
        OpAssignZfRShift -> do
            x <- infixOp env OpZfRShift v e
            setVar var x
        OpAssignBAnd -> do
            x <- infixOp env OpBAnd v e
            setVar var x
        OpAssignBOr -> do
            x <- infixOp env OpBOr v e
            setVar var x
        OpAssignBXor -> do
            x <- infixOp env OpBXor v e
            setVar var x

-- Call Function
evalExpr env (CallExpr name params) = do
    f <- evalExpr env name
    case f of
        Function _ args stmts -> do
            pushScope env
            createArgs env args params
            v <- evalStmt env (BlockStmt stmts)
            popScope env
            case v of
                Break -> error $ "Illegal break statement in function"
                Continue -> error $ "Illegal continue statement in function"
                Throw t -> return (Throw t)
                Return r -> return r
                NReturn -> return Nil
                _ -> return Nil
        Gvar ->
            do {
                Array vals <- evalExpr env (head params);
                Bool b <- equalsStr env (show name) "VarRef (Id \"len\")";
                if b && (null (tail params)) then return (Int (arraySize (Array vals))) else do
                Bool b <- equalsStr env (show name) "VarRef (Id \"head\")";
                if b && (null (tail params)) then return (head vals) else do
                Bool b <- equalsStr env (show name) "VarRef (Id \"tail\")";
                if b && (null (tail params)) then return (Array (tail vals)) else do
                Bool b <- equalsStr env (show name) "VarRef (Id \"concat\")";
                Array valsToConcat <- evalExpr env (head (tail params));
                if b && (null (tail(tail params))) then return (Array ((vals)++(valsToConcat))) else do
                Bool b <- equalsStr env (show name) "VarRef (Id \"compare\")";
                Array valsToCompare <- evalExpr env (head (tail params));
                if b && (null (tail(tail params))) then return (Bool (compareValues (Array vals) (Array valsToCompare)))
                else error $ "Variable " ++ show name ++ " not is a function"
            }
        _ -> error $ "Variable " ++ show name ++ " not is a function"
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Create the local parameter variables
createArgs :: StateT -> [Id] -> [Expression] -> StateTransformer Value
createArgs env [] [] = return Nil
createArgs env ((Id arg):xs) (param:ys) = do
    v <- evalExpr env param
    createLocalVar arg v
    createArgs env xs ys
createArgs env _ _ = error $ "Invalid amount of parameters"

-- Creating array from expressions
parseArray :: StateT -> [Expression] -> Value -> StateTransformer Value
parseArray env [] (Array []) = return $ Array []
parseArray env [] (Array vls) = return $ Array vls
parseArray env (expr:xs) (Array vls) = do
    v <- evalExpr env expr
    parseArray env xs (Array (vls ++ [v]))

-- Get a value from the index array
getArrayIndex :: StateT -> Value -> Value -> StateTransformer Value
getArrayIndex env (Array []) _ = error $ "Array index out of bounds"
getArrayIndex env (Array (x:xs)) index =
    case index of
        Int t -> if t < 0 then error $ "Negative index"
            else if t == 0 then return x else getArrayIndex env (Array xs) (Int (t - 1))
        _ -> error $ "Illegal argument type"

setArrayIndex :: StateT -> Value -> Value -> Value -> StateTransformer Value
setArrayIndex env (Array []) _ _ = error $ "Array index out of bounds"
setArrayIndex env (Array (x:xs)) index val =
    case index of
        Int t -> if t < 0 then error $ "Negative index"
            else if t == 0 then return (Array (val:xs))
            else do
                Array vals <- setArrayIndex env (Array xs) (Int (t - 1)) val
                return (Array (x:vals))
        _ -> error $ "Illegal argument type"

-- Create Object from properties
parseObject :: StateT -> [(Prop, Expression)] -> Value -> StateTransformer Value
parseObject env [] (Object []) = return (Object [])
parseObject env [] (Object atts) = return (Object atts)
parseObject env ((prop, expr):xs) (Object atts) =
    case prop of
    PropId (Id id) -> do
        v <- evalExpr env expr
        parseObject env xs (Object (atts ++ [IDType id v]))
    PropString str -> do
        v <- evalExpr env expr
        parseObject env xs (Object (atts ++ [STRType str v]))
    PropNum num -> do
        v <- evalExpr env expr
        parseObject env xs (Object (atts ++ [INTType num v]))

-- Gets an object property using dots, if have two or more equals properties, gets the first
getObjectDotProperty :: StateT -> Value -> Id -> StateTransformer Value
getObjectDotProperty env (Object []) (Id id) = error $ "Property not exists"
getObjectDotProperty env (Object (attr:xs)) (Id prop) = case attr of
    IDType id val -> do
        Bool b <- equalsStr env id prop
        if b then return val else getObjectDotProperty env (Object xs) (Id prop)
    STRType str val -> do
        Bool b <- equalsStr env str prop
        if b then return val else getObjectDotProperty env (Object xs) (Id prop)
    INTType num val -> getObjectDotProperty env (Object xs) (Id prop) -- DotRef cant catch number properties

-- Gets an object property unsing brackets, if have two or more equals properties, gets the first
getObjectBracketProperty :: StateT -> Value -> Value -> StateTransformer Value
getObjectBracketProperty env (Object []) prop = error $ "Property not exists"
getObjectBracketProperty env (Object (attr:xs)) prop =
    case prop of
        String str -> -- Strings can get all type of attributes
            case attr of
                IDType id val -> do
                    Bool b <- equalsStr env id str
                    if b then return val else getObjectBracketProperty env (Object xs) prop
                STRType s val -> do
                    Bool b <- equalsStr env s str
                    if b then return val else getObjectBracketProperty env (Object xs) prop
                INTType num val -> do
                    Bool b <- equalsStr env (show num) str
                    if b then return val else getObjectBracketProperty env (Object xs) prop
        Int int -> -- Ints just can get Int type attributes
            case attr of
                IDType id val -> getObjectBracketProperty env (Object xs) prop
                STRType str val -> getObjectBracketProperty env (Object xs) prop
                INTType num val -> do
                    Bool b <- equalsStr env (show num) (show int)
                    if b then return val else getObjectBracketProperty env (Object xs) prop
        _ -> error $ "Not implemented"


setObjectDotProperty :: StateT -> Value -> Id -> Value -> StateTransformer Value
-- If the property doesnt exists, the function creates the property
setObjectDotProperty env (Object []) (Id prop) val = return $ Object $ [IDType prop val]
setObjectDotProperty env (Object (attr:xs)) (Id prop) val = case attr of
    IDType id _ -> do
        Bool b <- equalsStr env id prop
        if b then return (Object ((IDType id val):xs)) else do
            Object attrs <- setObjectDotProperty env (Object xs) (Id prop) val
            return (Object (attr:attrs))
    STRType str _ -> do
        Bool b <- equalsStr env str prop
        if b then return (Object ((STRType str val):xs)) else do
            Object attrs <- setObjectDotProperty env (Object xs) (Id prop) val
            return (Object (attr:attrs))
    INTType num _ -> do -- dot refer dont work with num properties
        Object attrs <- setObjectDotProperty env (Object xs) (Id prop) val
        return (Object (attr:attrs))

setObjectBracketProperty :: StateT -> Value -> Value -> Value -> StateTransformer Value
-- If the property doesnt exists, the function creates the property
setObjectBracketProperty env (Object []) prop val =
    case prop of
        String str -> return $ Object $ [IDType str val]
        Int int -> return $ Object $ [INTType (toInteger int) val]
setObjectBracketProperty env (Object (attr:xs)) prop val =
    case prop of
        String str -> case attr of
            IDType id _ -> do
                Bool b <- equalsStr env id str
                if b then return (Object ((IDType id val):xs)) else do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
            STRType s _ -> do
                Bool b <- equalsStr env str s
                if b then return (Object ((STRType str val):xs)) else do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
            INTType num _ -> do -- dot refer dont work with num properties
                Bool b <- equalsStr env str (show num)
                if b then return (Object ((INTType num val):xs)) else do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
        Int int -> case attr of
            IDType id _ -> do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
            STRType s _ -> do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
            INTType num _ -> do -- dot refer dont work with num properties
                Bool b <- equalsStr env (show int) (show num)
                if b then return (Object ((INTType num val):xs)) else do
                    Object attrs <- setObjectBracketProperty env (Object xs) prop val
                    return (Object (attr:attrs))
        _ -> error $ "Illegal property argument"

-- Verify if two strings are equals
equalsStr :: StateT -> String -> String -> StateTransformer Value
equalsStr env [] [] = return $ Bool True
equalsStr env (c1:xs) (c2:ys) = if c1 == c2 then equalsStr env xs ys else return $ Bool False
equalsStr _ _ _ = return $ Bool False

-- Value comparator
compareValues :: Value -> Value -> Bool
compareValues (Int x) (Int y) = x == y
compareValues (String x) (String y) = x == y
compareValues (Var x) (Var y) = x == y
compareValues (Object x) (Object y) = (compareObject (Object x) (Object y)) 
compareValues (Array x) (Array y) = (compareArray (Array x) (Array y))
compareValues _ _ = error $ "Unknown Type"

compareArray (Array []) (Array []) = True
compareArray (Array (x:xs)) (Array (y:ys)) = if (compareValues x y) then (compareArray (Array xs) (Array ys)) else False
compareArray _ _ = False

compareObject (Object []) (Object []) = True
compareObject (Object (x:xs)) (Object (y:ys)) = if (compareAttribute x y) then (compareObject (Object xs) (Object ys)) else False
compareObject _ _ = False

compareAttribute (IDType s1 v1) (IDType s2 v2) = if (s1 == s2) && (compareValues v1 v2) then True else False
compareAttribute (STRType s1 v1) (STRType s2 v2) = if (s1 == s2) && (compareValues v1 v2) then True else False
compareAttribute (INTType i1 v1) (INTType i2 v2) = if (i1 == i2) && (compareValues v1 v2) then True else False
compareAttribute _ _ = False

arraySize (Array []) = 0
arraySize (Array (x:xs)) = (1 + (arraySize (Array xs)))
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
        Return r -> return (Return r)
        NReturn -> return NReturn
        _ -> evalStmt env (BlockStmt stmts)

-- If
evalStmt env (IfSingleStmt expr stmt) = do
    Bool v <- evalExpr env expr
    if v then do
        --pushScope env
        a <- evalStmt env stmt
        --popScope env
        return a
    else return Nil

--If Else
evalStmt env (IfStmt expr stmt1 stmt2) = do
    Bool v <- evalExpr env expr
    if v then do
        --pushScope env
        a <- evalStmt env stmt1
        --popScope env
        return a
    else do
        --pushScope env
        a <- evalStmt env stmt2
        --popScope env
        return a

-- SwitchStmt Expression [CaseClause]
-- Switch
-- Evaluate the clause, because can be an unary assign
evalStmt env (SwitchStmt expr []) = evalExpr env expr
evalStmt env (SwitchStmt expr (cl:xs)) = do
    a <- evalExpr env expr
    case cl of
        CaseClause cexpr stmts -> do
            ac <- evalExpr env cexpr
            Bool b <- infixOp env OpEq a ac
            if b then do -- Multiple switch clauses uses the same scope
                --pushScope env
                v <- evalClause env (cl:xs)
                --popScope env
                return v
            else evalStmt env (SwitchStmt expr xs)

-- While
evalStmt env (WhileStmt expr stmt) = do
    Bool b <- evalExpr env expr
    if b then do 
        --pushScope env
        v <- evalStmt env stmt
        --popScope env
        case v of
            Break -> return Nil
            Throw t -> return (Throw t)
            Return r -> return (Return r)
            NReturn -> return NReturn
            Continue -> evalStmt env (WhileStmt expr stmt)
            _ -> evalStmt env (WhileStmt expr stmt)
        
    else return Nil

-- Do While
evalStmt env (DoWhileStmt stmt expr) = do
    --pushScope env
    v <- evalStmt env stmt
    Bool b <- evalExpr env expr
    --popScope env
    case v of
        Break -> return Nil
        Throw t -> return (Throw t)
        Return r -> return (Return r)
        NReturn -> return NReturn
        Continue -> if b then evalStmt env (DoWhileStmt stmt expr) else return Nil
        _ -> if b then evalStmt env (DoWhileStmt stmt expr) else return Nil

-- For
evalStmt env (ForStmt init test inc stmt) = forBegin env init test inc stmt

-- For in
evalStmt env (ForInStmt (ForInVar (Id id)) objexpr stmt) = do
    obj <- evalExpr env objexpr
    forInLoop env id obj stmt
evalStmt env (ForInStmt (ForInLVal (LVar id)) objexpr stmt) = do
    obj <- evalExpr env objexpr
    forInLoop env id obj stmt

-- Break
evalStmt env (BreakStmt m) = return Break;

-- Continue
evalStmt env (ContinueStmt m) = return Continue;

-- Throw
evalStmt env (ThrowStmt expr) = do
    v <- evalExpr env expr
    return (Throw v)

-- Try - Catch and finally can read the throw scope vars, but finally cant read the catch vars, so, catch needs of a new scope
evalStmt env (TryStmt stmt catch finally) = do
    --pushScope env
    v <- evalStmt env stmt
    case v of
        Break -> return Break
        Throw t ->
            case catch of
                Nothing -> return Nil
                Just (CatchClause (Id id) cstmt) -> do
                    --pushScope env
                    createLocalVar id t
                    evalStmt env cstmt
                    --popScope env
        Return r -> return (Return r)
        NReturn -> return NReturn
        Continue -> return Continue
        _ -> return Nil
    case finally of
        Nothing -> return Nil --popScope env
        Just fstmt -> do
            v <- evalStmt env fstmt
            --popScope env
            return v

-- functions
-- saving a function as a value
evalStmt env (FunctionStmt (Id name) args stmts) = createGlobalVar name (Function (Id name) args stmts)

-- Return in functions
evalStmt env (ReturnStmt mexpr) =
    case mexpr of
        Nothing -> return NReturn
        Just val -> do
            v <- evalExpr env val
            return (Return v)

---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Switch support function 
evalClause :: StateT -> [CaseClause] -> StateTransformer Value
evalClause env [] = return Nil
evalClause env (cl:xs) = do
    case cl of
        CaseClause expr stmts -> do
            v <- evalStmt env (BlockStmt stmts)
            case v of
                Break -> return Nil
                Continue -> error $ "Illegal Statement"
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return Nil
                _ -> evalClause env xs
        CaseDefault stmts -> do
            v <- evalStmt env (BlockStmt stmts)
            case v of
                Break -> return Nil
                Continue -> error $ "Illegal Statement"
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return Nil
                _ -> evalClause env xs


-- For in support function
forInLoop :: StateT -> String -> Value -> Statement -> StateTransformer Value
forInLoop env id (Array []) stmt = return Nil
forInLoop env id (Array (val:xs)) stmt = do
    --pushScope env
    createLocalVar id val
    v <- evalStmt env stmt
    --popScope env
    case v of
        Break -> return Nil
        Throw t -> return (Throw t)
        Return r -> return (Return r)
        NReturn -> return NReturn
        Continue -> forInLoop env id (Array xs) stmt
        _ -> forInLoop env id (Array xs) stmt

forInLoop env id (Object []) stmt = return Nil
forInLoop env id (Object (attr:xs)) stmt = do
    case attr of
        IDType i val -> do
            --pushScope env
            createLocalVar id (String i)
            v <- evalStmt env stmt
            --popScope env
            case v of
                Break -> return Nil
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return NReturn
                Continue -> forInLoop env id (Object xs) stmt
                _ -> forInLoop env id (Object xs) stmt
        STRType s val -> do
            --pushScope env
            createLocalVar id (String s)
            v <- evalStmt env stmt
            --popScope env
            case v of
                Break -> return Nil
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return NReturn
                Continue -> forInLoop env id (Object xs) stmt
                _ -> forInLoop env id (Object xs) stmt
        INTType n val -> do
            --pushScope env
            createLocalVar id (Int (fromInteger n))
            v <- evalStmt env stmt
            --popScope env
            case v of
                Break -> return Nil
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return NReturn
                Continue -> forInLoop env id (Object xs) stmt
                _ -> forInLoop env id (Object xs) stmt
forInLoop _ _ x _ = error $ "Illegal for in type"


-- For support functions
forBegin :: StateT -> ForInit -> Maybe Expression -> Maybe Expression -> Statement -> StateTransformer Value
forBegin env init test inc stmt = do
    --pushScope env
    case init of
        NoInit -> return Nil
        VarInit vars -> evalStmt env (VarDeclStmt vars) -- Create local vars
        ExprInit expr -> evalExpr env expr
    case test of
        Nothing -> do
            --pushScope env
            v <- evalStmt env stmt
            --popScope env
            case v of
                Break -> do
                    --popScope env
                    return Nil
                Throw t -> do
                    --popScope env
                    return (Throw t)
                Return r -> do
                    --popScope env
                    return (Return r)
                NReturn -> do
                    --popScope env
                    return NReturn
                Continue -> do
                    forContinue env init test inc stmt
                    --popScope env
                _ -> do
                    forContinue env init test inc stmt
                    --popScope env
        Just expr -> do
            Bool b <- evalExpr env expr
            if b then do
                --pushScope env
                v <- evalStmt env stmt
                --popScope env
                case v of
                    Break -> do
                        --popScope env
                        return Nil
                    Throw t -> do
                        --popScope env
                        return (Throw t)
                    Return r -> do
                        --popScope env
                        return (Return r)
                    NReturn -> do
                        --popScope env
                        return NReturn
                    Continue -> do
                        forContinue env init test inc stmt
                        --popScope env
                    _ -> do
                        forContinue env init test inc stmt
                        --popScope env
            else return Nil --popScope env

forContinue :: StateT -> ForInit -> Maybe Expression -> Maybe Expression -> Statement -> StateTransformer Value
forContinue env init test inc stmt = do
    case inc of
        Nothing -> return Nil
        Just expr -> evalExpr env expr
    case test of
        Nothing -> do
            --pushScope env
            v <- evalStmt env stmt
            --popScope env
            case v of
                Break -> return Break
                Throw t -> return (Throw t)
                Return r -> return (Return r)
                NReturn -> return NReturn
                Continue -> forContinue env init test inc stmt
                _ -> forContinue env init test inc stmt
        Just expr -> do
            Bool b <- evalExpr env expr
            if b then do
                --pushScope env
                v <- evalStmt env stmt
                --popScope env
                case v of
                    Break -> return Break
                    Throw t -> return (Throw t)
                    Return r -> return (Return r)
                    NReturn -> return NReturn
                    Continue -> forContinue env init test inc stmt
                    _ -> forContinue env init test inc stmt
            else return Nil
---------------------------------------------------------------------------------------------------

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

---------------------------------------------------------------------------------------------------
-- Implementation of prefix operations
prefixOp :: StateT -> PrefixOp -> Value -> StateTransformer Value
prefixOp env PrefixLNot   (Int  v) = return $ Int  $ (-v)
prefixOp env PrefixMinus  (Int  v) = return $ Int  $ (-v)
prefixOp env PrefixBNot   (Bool  v) = return $ Bool  $ not v
---------------------------------------------------------------------------------------------------


infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2

---------------------------------------------------------------------------------------------------
infixOp env OpLShift    (Int  v1) (Int  v2) = return $ Int  $ shiftL v1 v2
infixOp env OpSpRShift  (Int  v1) (Int  v2) = return $ Int  $ shiftR v1 v2
-- OpZfRShift >>> Logical right shitf not implemented
infixOp env OpZfRShift  (Int  v1) (Int  v2) = error $ "Operation not implemented"
---------------------------------------------------------------------------------------------------


infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
---------------------------------------------------------------------------------------------------
infixOp env OpNEq  (Int  v1) (Int  v2) = return $ Bool $ v1 /= v2
---------------------------------------------------------------------------------------------------
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
---------------------------------------------------------------------------------------------------
infixOp env OpBAnd (Int  v1) (Int  v2) = error $ "Operation not implemented"
infixOp env OpBOr  (Int  v1) (Int  v2) = error $ "Operation not implemented"
infixOp env OpBXor (Int  v1) (Int  v2) = error $ "Operation not implemented"
---------------------------------------------------------------------------------------------------

infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

---------------------------------------------------------------------------------------------------
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
updateVar _ _ [] = error $ "Unreachable error"
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
---------------------------------------------------------------------------------------------------

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
