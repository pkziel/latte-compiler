-- read top -> bottom

module TypeChecker (
    typeCheck, Liner
) where

import qualified Data.Map as M
import ErrM
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import System.IO

import AbsLatte
import Utils

type Liner = Maybe (Int, Int)                       -- place/line in code added to tree
type VStore a = [(VEnv a)]                          -- context stack
type FEnv a = M.Map Ident ([(Type a)], (Type a))    -- function type signature
type VEnv a = M.Map Ident (Type a)                  -- variables with their types
type Mem a b = ReaderT (FEnv b) (ErrorT String (StateT (VStore b) IO)) a

typeCheck :: (Program Liner) -> IO()
typeCheck (Program _ topDefs) = case getFunctionsDef topDefs of
    Bad err -> throwMyError err
    Ok fenv -> runCheckFunction fenv topDefs

-- firstly add functions to env
getFunctionsDef :: [(TopDef Liner)] -> Err (FEnv Liner)
getFunctionsDef topDefs = do
    fenv <- foldM (updateFun) emptyFEnv topDefs
    case M.lookup (Ident "main") fenv of
        Nothing -> errNoMain
        Just ([], (Int _)) -> return fenv
        _ -> errBadSignature

-- -- predifined functions added to env during init
emptyFEnv :: (FEnv Liner)
emptyFEnv = M.fromList [
        (Ident "concat", ([Str Nothing, Str Nothing], Str Nothing)),
        (Ident "printInt", ([Int Nothing], Void Nothing)),
        (Ident "printString", ([Str Nothing], Void Nothing)),
        (Ident "error", ([], Void Nothing)),
        (Ident "readInt", ([], Int Nothing)),
        (Ident "readString", ([], Str Nothing))] 

updateFun :: (FEnv Liner) -> (TopDef Liner) -> Err (FEnv Liner)
updateFun fenv (FnDef line t id@(Ident s) args _) = case M.lookup id fenv of
    Just _ -> errFunctionExists s line
    Nothing -> case checkNullArgs args of 
        True -> fail $ "Function can't take void parameters" ++ (addLine line)
        False -> return $ (M.insert id ( types args, t) fenv)
    where
        types _args = map (\(Arg _ t _) -> t) _args

checkNullArgs :: [(Arg Liner)] -> Bool
checkNullArgs [] = False
checkNullArgs ((Arg _ b _) : t ) = case b of
    (Void _) -> True
    _ -> checkNullArgs t

-- -- secondly we can check body of evsery single function
runCheckFunction :: (FEnv Liner) -> [(TopDef Liner)] -> IO()
runCheckFunction _ [] = return ()
runCheckFunction fenv (h:t) = do
    r <- evalStateT (runErrorT (runReaderT (checkFunction h) fenv)) []
    case r of
        Left err -> throwMyError err
        Right _ -> runCheckFunction fenv t
  
checkFunction :: (TopDef Liner) -> Mem () Liner
checkFunction (FnDef line t id@(Ident s) args (Block _ stmts)) = do
    store <- foldM (addArg s) M.empty args
    put [store]
    returnsProperly <- foldM (checkStmt t) (shouldRet t) stmts
    case returnsProperly of
        True -> return ()
        False -> errFunctionShouldReturn s t line

shouldRet :: Type Liner -> Bool
shouldRet (Void _) = True
shouldRet _ = False

addArg :: String -> (VEnv Liner) -> (Arg Liner) -> Mem (VEnv Liner) Liner
addArg fun_id m (Arg line t id@(Ident id_)) = case (M.lookup id m) of
    Nothing -> return (M.insert id t m)
    Just _ -> errArgsWithSameName id_ fun_id line

checkStmt :: (Type Liner) -> Bool -> (Stmt Liner) -> Mem Bool Liner
checkStmt _ b (Empty _) = return b
checkStmt t b (BStmt _ (Block _ block)) = do
    s <- get
    put (M.empty:s)
    b <- foldM (checkStmt t) b block
    put s
    return b
checkStmt _ b (Decl line t items) = do
    case t of
        (Void _) -> fail $ "Declaring variable with void type" ++ (addLine line)
        _ -> do
            forM items (checkItem t)
            return b
checkStmt _ b (Ass line id@(Ident i) exp) = do 
    exp_type <- infer exp
    s <- get
    case findTypeOfIdInStack id s of
        Bad err -> errVarNotDecl i "assignment" line
        Ok id_type -> case (sameType id_type exp_type) of
            True -> return b
            False -> errAssType i id_type exp_type line
checkStmt _ b (Incr line id@(Ident i)) = do 
    s <- get
    case findTypeOfIdInStack id s of    
        Bad err -> errVarNotDecl i "incrementation" line
        Ok id_type -> case (isInt id_type) of
            True -> return b
            False -> errActionBadType "Incrementing" i line
checkStmt _ b (Decr line id@(Ident i)) = do 
    s <- get
    case findTypeOfIdInStack id s of    
        Bad err -> errVarNotDecl i "decrementation" line
        Ok id_type -> case (isInt id_type) of
            True -> return b
            False -> errActionBadType "Decrementing" i line
checkStmt t _ (Ret line exp) = do
    exp_type <- infer exp
    case exp_type of
        (Void _) -> fail $ "Returning void value" ++ addLine(line)
        _ -> case (sameType t exp_type) of
            True -> return True
            False -> errExpectedReturnType t (showMy exp_type) line
checkStmt t _ (VRet line) =
    case (isVoid t) of
        True -> return True
        False -> errExpectedReturnType t "void" line
checkStmt t b (Cond line expr stmt) = do
    exp_type <- infer expr
    case (isBool exp_type) of
        False -> errNonBoolIn "if" line
        True -> do
            was_ret <- foldM (checkStmt t) b [stmt]
            case (evalConst expr) of
                Bad err -> return b 
                Ok const_bool -> return (b || (const_bool && was_ret))
checkStmt t b (CondElse line expr stmt1 stmt2) = do 
    exp_type <- infer expr
    case (isBool exp_type) of
        False -> errNonBoolIn "if-else" line
        True -> do
            b1 <- foldM (checkStmt t) b [stmt1]
            b2 <- foldM (checkStmt t) b [stmt2]
            case (evalConst expr) of
                Bad err -> return (b || (b1 && b2))
                Ok const_bool -> --only one branch can be choosen
                    return (b || (const_bool && b1) || (not const_bool && b2)) 
checkStmt t b (While line exp stmt) = do
    exp_type <- infer exp
    case (isBool exp_type) of
        False -> errNonBoolIn "while" line
        True -> do
            was_ret <- foldM (checkStmt t) b [stmt]
            case (evalConst exp) of
                Bad err -> return b 
                Ok const_bool -> return (b || (const_bool && was_ret))
checkStmt _ b (SExp _ exp) = do
    exp_type <- infer exp
    return b



checkItem :: (Type Liner) -> (Item Liner) -> Mem () Liner
checkItem t item@(NoInit line ident@(Ident id)) = do
    s <- get
    case (findTypeOfIdInStack ident [head s]) of
        Ok _ -> errVarAlreadyDecl id line
        Bad err -> do
            put ((M.insert ident t (head s)):(tail s)) 
            return ()
checkItem t (Init line ident@(Ident id) exp) = do
    s <- get
    case (findTypeOfIdInStack ident [head s]) of
        Ok _ -> errVarAlreadyDecl id line
        Bad err -> do
            exp_type <- infer exp
            case (sameType exp_type t) of
                False -> errDeclInitializer id t exp_type line
                True -> do
                    put ((M.insert ident t (head s)):(tail s)) 
                    return ()

findTypeOfIdInStack :: Ident -> (VStore Liner) -> Err (Type Liner)
findTypeOfIdInStack id [] = fail "~I just propagate err"
findTypeOfIdInStack id (h:t) = case M.lookup id h of
    Nothing -> findTypeOfIdInStack id t
    Just type_ -> return type_  

infer :: (Expr Liner) -> Mem (Type Liner) Liner
infer (ELitTrue _) = return (Bool Nothing)
infer (ELitFalse _) = return (Bool Nothing)
infer (EString _ _) = return (Str Nothing)
infer (ELitInt line i) = case (i >  2147483647 || i < -2147483648) of
    True -> fail $ "Int const used in exp is not in range (-2147483648, 2147483647) " ++ (addLine line)
    False -> return (Int Nothing)
infer (Not line exp) = do
    exp_type <- infer exp
    case (isBool exp_type) of
        True -> return (Bool Nothing)
        False -> fail $ "Applying '!'' to non bool expression" ++ (addLine line)
infer (Neg line exp) = do
    exp_type <- infer exp
    case (isInt exp_type) of
        True -> return (Int Nothing)
        False -> fail $ "Negating non int expression" ++ (addLine line)
infer (EVar line ident@(Ident i)) = do
    s <- get
    case findTypeOfIdInStack ident s of    
        Bad err -> fail $ "Variable '" ++ i ++ "' was not declared but used in expression"
            ++ addLine(line)
        Ok id_type -> return id_type
infer (EApp line ident@(Ident i) exps) = do
    (res) <- asks (M.lookup ident)
    case res of 
        Nothing -> errFunNotDecl i line
        Just (types, ret_typ) -> case (length types) == (length exps) of
            False -> errWrongNumberPar i line
            True -> do
                checkFunctionArgs types exps i line
                return ret_typ
infer (EMul line e1 _ e2) = do
    type1 <- infer e1
    type2 <- infer e2
    case (isInt type1 && isInt type2) of
        False -> fail $ "Both left and right expression has to be int in times/div/mul exp"
            ++ (addLine line)
        True -> return (Int Nothing)        
infer (EAdd line e1 (Plus _) e2) = do
    type1 <- infer e1
    type2 <- infer e2
    case (isInt type1 && isInt type2 || isStr type1 && isStr type2) of
        False -> fail $ "Both left and right expression has to be int or string in add exp"
            ++ (addLine line)
        True -> return type1
infer (EAdd line e1 (Minus _) e2) = do
    type1 <- infer e1
    type2 <- infer e2
    case (isInt type1 && isInt type2) of
        False -> fail $ "Both left and right expression has to be int in sub exp"
            ++ (addLine line)
        True -> return type1
infer (EAnd line e1 e2) = do
    type1 <- infer e1
    type2 <- infer e2
    case (isBool type1 && isBool type2) of
        False -> fail $ "Both left and right expression has to be bool in and-exp"
            ++ (addLine line)
        True -> return type1
infer (EOr line e1 e2) = do
    type1 <- infer e1
    type2 <- infer e2
    case (isBool type1 && isBool type2) of
        False -> fail $ "Both left and right expression has to be bool in or-exp"
            ++ (addLine line)
        True -> return type1
infer (ERel line e1 (EQU _) e2) = do    
    type1 <- infer e1
    type2 <- infer e2
    case (sameType type1 type2) of
        True -> return (Bool Nothing)
        False -> fail $ "Both left and right expression has to be same in == exp" 
            ++ (addLine line)
infer (ERel line e1 (NE _) e2) = do    
    type1 <- infer e1
    type2 <- infer e2
    case (sameType type1 type2) of
        True -> return (Bool Nothing)
        False -> fail $ "Both left and right expression has to be same != exp" 
            ++ (addLine line)
infer (ERel line e1 _ e2) = do    
    type1 <- infer e1
    type2 <- infer e2
    case (isInt type1 && isInt type2) of
        True -> return (Bool Nothing)
        False -> fail $ "Both left and right expression has to be int <,<=,>,=> exps" 
            ++ (addLine line)

checkFunctionArgs:: [(Type Liner)] -> [(Expr Liner)] -> String -> Liner -> Mem () Liner
checkFunctionArgs [] [] _ _ = return ()
checkFunctionArgs (h:t) (x:s) str line = do
    app_type <- infer x
    case (sameType h app_type) of
        True -> checkFunctionArgs t s str line
        False -> errWrongParType str h app_type line

-- eval const expr based on true/false, can be extended one day 
evalConst :: (Expr Liner) -> Err Bool
evalConst (ELitTrue _) = return True
evalConst (ELitFalse _) = return False
evalConst (EOr _ e1 e2) = do
    b1 <- evalConst e1
    b2 <- evalConst e2
    return (b1 || b2)
evalConst (EAnd _ e1 e2) = do
    b1 <- evalConst e1
    b2 <- evalConst e2
    return (b1 && b2)
evalConst (ERel line e1 (NE _) e2)  = do
    b1 <- evalConst e1
    b2 <- evalConst e2
    return $ not (b1 == b2)
evalConst (ERel line e1 (EQU _) e2)  = do
    b1 <- evalConst e1
    b2 <- evalConst e2
    return (b1 == b2)
evalConst _ = fail "~I just propagate err"