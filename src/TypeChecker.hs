-- author: Piotr Zieliński
-- read top -> bottom

module TypeChecker (
    typeCheck
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

type VStore a = [(VEnv a)]                          -- context stack
type FEnv a = M.Map Ident ([(Type a)], (Type a))    -- function type signature
type VEnv a = M.Map Ident (Type a)                  -- variables with their types
type Mem a b = ReaderT (FEnv b) (ErrorT String (StateT (VStore b) IO)) a

typeCheck :: (Program Liner) -> IO()
typeCheck (Program _ topDefs) = case getFunctionsDef topDefs of
    Bad err -> putErrorInStderr err
    Ok fenv -> runCheckFunction fenv topDefs

getFunctionsDef :: [(TopDef Liner)] -> Err (FEnv Liner)
getFunctionsDef topDefs = do
    fenv <- foldM (updateFun) emptyFEnv topDefs
    case M.lookup (Ident "main") fenv of
        Nothing -> fail "No function named 'main'"
        Just ([], (Int _)) -> return fenv
        _ -> fail "Bad signature for 'main' function"

-- -- predifined functions added to env during init
emptyFEnv :: (FEnv Liner)
emptyFEnv = M.fromList [
        (Ident "printInt", ([Int Nothing], Void Nothing)),
        (Ident "printString", ([Str Nothing], Void Nothing)),
        (Ident "error", ([], Void Nothing)),
        (Ident "readInt", ([], Int Nothing)),
        (Ident "readString", ([], Str Nothing))] 

updateFun :: (FEnv Liner) -> (TopDef Liner) -> Err (FEnv Liner)
updateFun fenv (FnDef line t id@(Ident s) args _) = case M.lookup id fenv of
    Nothing -> return $ (M.insert id ( types args, t) fenv)
    Just _ -> fail $ "Function with name '" ++ s ++ "' was already declared" ++
        (addLine line)
    where 
        types _args = map (\(Arg _ t _) -> t) _args

-- -- secondly we can check body of evsery single function
runCheckFunction :: (FEnv Liner) -> [(TopDef Liner)] -> IO()
runCheckFunction _ [] = putStrLn ("Ok\n")
runCheckFunction fenv (h:t) = do
    r <- evalStateT (runErrorT (runReaderT (checkFunction h) fenv)) []
    case r of
        Left err -> putErrorInStderr err
        Right _ -> runCheckFunction fenv t
  
checkFunction :: (TopDef Liner) -> Mem () Liner
checkFunction (FnDef line t id@(Ident s) args (Block _ stmts)) = do
    store <- foldM (addArg s) M.empty args
    put [store]
    returnsProperly <- foldM (checkStmt t) (shouldRet t) stmts
    case returnsProperly of
        True -> return ()
        False -> fail $ "Function '" ++ s ++ "' should return parameter " ++ 
            (showMy t) ++ (addLine line)

shouldRet :: Type Liner -> Bool
shouldRet (Void _) = True
shouldRet _ = False

addArg :: String -> (VEnv Liner) -> (Arg Liner) -> Mem (VEnv Liner) Liner
addArg fun_id m (Arg line t id@(Ident id_)) = case (M.lookup id m) of
    Nothing -> return (M.insert id t m)
    Just _ -> fail $ "Two arguments with same name '" ++ id_ ++ "' in function '" ++ 
        fun_id ++ "'" ++ (addLine line)


checkStmt :: (Type Liner) -> Bool -> (Stmt Liner) -> Mem Bool Liner
checkStmt _ b (Empty _) = return b
checkStmt t b (BStmt _ (Block _ block)) = do
    s <- get
    put (M.empty:s)
    was_return <- foldM (checkStmt t) b block
    return was_return
checkStmt _ b (Decl _ t items) = do
   forM items (checkItem t)
   return b
checkStmt _ b (Ass line id@(Ident i) exp) = do 
    exp_type <- infer exp
    s <- get
    case findTypeOfIdInStack id s of
        Bad err -> fail $ "Variable with id '" ++ i ++ "' was not declared before assignment" ++
            (addLine line)
        Ok id_type -> case (sameType id_type exp_type) of
            True -> return b
            False -> fail $ "Assignment expression with wrong type to variable '" ++ i ++ 
                "'\nExpected type: " ++ (showMy id_type) ++ "\nGot type: " ++ (showMy exp_type)
                ++ (addLine line)
checkStmt _ b (Incr line id@(Ident i)) = do 
    s <- get
    case findTypeOfIdInStack id s of    
        Bad err -> fail $ "Variable '" ++ i ++ "' was not declared before incrementation" 
            ++ (addLine line)
        Ok id_type -> case (isInt id_type) of
            True -> return b
            False -> fail $ "Incrementing variable '" ++ i ++ "'' which is non Int type"
                ++ (addLine line)
checkStmt _ b (Decr line id@(Ident i)) = do 
    s <- get
    case findTypeOfIdInStack id s of    
        Bad err -> fail $ "Variable '" ++ i ++ "' was not declared before decrementation" 
            ++ (addLine line)
        Ok id_type -> case (isInt id_type) of
            True -> return b
            False -> fail $ "Decrementing variable '" ++ i ++ "'' which is non Int type"
                ++ (addLine line)
checkStmt t _ (Ret line exp) = do
    exp_type <- infer exp
    case (sameType t exp_type) of
        True -> return True
        False -> fail $ "Expected return type: " ++ (showMy t) ++ "\nReached: " 
            ++ (showMy exp_type) ++ (addLine line)
checkStmt t _ (VRet line) =
    case (isVoid t) of
        True -> return True
        False -> fail $ "Expected return type: " ++ (showMy t) ++ "\nReached: void"
            ++ (addLine line)
checkStmt t b (Cond line expr stmt) = do
    exp_type <- infer expr
    case (isBool exp_type) of
        False -> fail $ "Non bool expression in if condition" ++ (addLine line)
        True -> do
            foldM (checkStmt t) b [stmt]
            return b 
checkStmt t b (CondElse line expr stmt1 stmt2) = do 
    exp_type <- infer expr
    case (isBool exp_type) of
        False -> fail $ "Non bool expression in if-else condition" ++ (addLine line)
        True -> do
            b1 <- foldM (checkStmt t) b [stmt1]
            b2 <- foldM (checkStmt t) b [stmt2]
            return (b || (b1 && b2))
checkStmt t b (While line exp stmt) = do
    exp_type <- infer exp
    case (isBool exp_type) of
        False -> fail $ "Non bool expression in while condition" ++ (addLine line)
        True -> do
            foldM (checkStmt t) b [stmt]
            return b     
checkStmt _ b (SExp _ exp) = do
    exp_type <- infer exp
    return b

checkItem :: (Type Liner) -> (Item Liner) -> Mem () Liner
checkItem t item@(NoInit line ident@(Ident id)) = do
    s <- get
    case (findTypeOfIdInStack ident [head s]) of
        Bad err -> do
            put ((M.insert ident t (head s)):(tail s)) 
            return ()
        Ok _ -> fail $ "Variable '" ++ id ++ "' was already declared in this scope" ++
            (addLine line)
checkItem t (Init line ident@(Ident id) exp) = do
    s <- get
    case (findTypeOfIdInStack ident [head s]) of
        Bad err -> do
            exp_type <- infer exp
            case (sameType exp_type t) of
                True -> do
                    put ((M.insert ident t (head s)):(tail s)) 
                    return ()
                False -> fail $ "Declaring variable '" ++ id ++ 
                    "' with wrong initializer type" ++ "\nExpected type: " ++ showMy(t)
                    ++ "\nGot type: " ++ (showMy exp_type) ++ (addLine line)
        Ok _ -> fail $ "Variable '" ++ id ++ "' was already declared in this scope" ++
            (addLine line)

findTypeOfIdInStack :: Ident -> (VStore Liner) -> Err (Type Liner)
findTypeOfIdInStack id [] = fail "~I just propagate err / if seen concact with developer"
findTypeOfIdInStack id (h:t) = case M.lookup id h of
    Nothing -> findTypeOfIdInStack id t
    Just type_ -> return type_  

infer :: (Expr Liner) -> Mem (Type Liner) Liner
infer (ELitTrue _) = return (Bool Nothing)
infer (ELitFalse _) = return (Bool Nothing)
infer (EString _ _) = return (Str Nothing)
infer (ELitInt _ _) = return (Int Nothing)
infer (Not _ exp) = do
    exp_type <- infer exp
    case (isBool exp_type) of
        True -> return (Bool Nothing)
        False -> fail $ "Applying '!'' to non bool expression"
infer (Neg _ exp) = do
    exp_type <- infer exp
    case (isInt exp_type) of
        True -> return (Int Nothing)
        False -> fail $ "Negating non int expression"
infer (EVar _ ident@(Ident i)) = do
    s <- get
    case findTypeOfIdInStack ident s of    
        Bad err -> fail $ "Variable " ++ i ++ " was not declared but used in expression"
        Ok id_type -> return id_type
infer (EApp _ ident@(Ident i) exps) = do
    (res) <- asks (M.lookup ident)
    case res of 
        Nothing -> fail $ "Function named " ++ i ++  " was not declared but used in expression"
        Just (types, ret_typ) -> case (length types) == (length exps) of
            False -> fail $ "Wrong number of parameters in function " ++ i ++ " application "
            True -> do
                checkFunctionArgs types exps i
                return ret_typ
infer (EMul _ exp1 _ exp2) = do
    type1 <- infer exp1
    type2 <- infer exp2
    case (isInt type1 && isInt type2) of
        False -> fail $ "Both left and right expression has to be int in mul exp"
        True -> return (Int Nothing)        
infer (EAdd _ exp1 (Plus _) exp2) = do
    type1 <- infer exp1
    type2 <- infer exp2
    case (isInt type1 && isInt type2 || isStr type1 && isStr type2) of
        False -> fail $ "Both left and right expression has to be int or string in add exp"
        True -> return type1
infer (EAdd _ exp1 (Minus _) exp2) = do
    type1 <- infer exp1
    type2 <- infer exp2
    case (isInt type1 && isInt type2) of
        False -> fail $ "Both left and right expression has to be int in minus exp"
        True -> return type1
infer (EAnd _ exp1 exp2) = do
    type1 <- infer exp1
    type2 <- infer exp2
    case (isBool type1 && isBool type2) of
        False -> fail $ "Both left and right expression has to be bool in and-exp"
        True -> return type1
infer (EOr _ exp1 exp2) = do
    type1 <- infer exp1
    type2 <- infer exp2
    case (isBool type1 && isBool type2) of
        False -> fail $ "Both left and right expression has to be bool in or-exp"
        True -> return type1
infer (ERel _ expr1 _ expr2) = do    
    type1 <- infer expr1
    type2 <- infer expr2
    case (sameType type1 type2) of
        False -> fail $ "Both left and right expression has to be same relop-exp"
        True -> return (Bool Nothing)

checkFunctionArgs:: [(Type Liner)] -> [(Expr Liner)] -> String -> Mem () Liner
checkFunctionArgs [] [] _ = return ()
checkFunctionArgs (h:t) (x:s) str = do
    app_type <- infer x
    case (sameType h app_type) of
        True -> checkFunctionArgs t s str
        False -> fail $ "Wrong type parameter in function " ++ str ++ "application" ++
            "\nExpected type: " ++ (show h) ++ "\nGot type: " ++ show(app_type)