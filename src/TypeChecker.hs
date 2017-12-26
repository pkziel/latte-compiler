-- author: Piotr Zieliński
-- read top -> bottom
-- TODO make messages more informative with same format 

module TypeChecker (
    typeCheck
) where

import qualified Data.Map as M
import ErrM
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import System.Exit
import System.IO

import AbsLatte
import Utils

type VStore a = [(VEnv a)]                          -- context stack
type FEnv a = M.Map Ident ([(Type a)], (Type a))    -- function type signature
type VEnv a = M.Map Ident (Type a)                  -- variables with their types
type Mem a b = ReaderT (FEnv b) (ErrorT String (StateT (VStore b) IO)) a -- works fine do not read
type Liner = Maybe (Int, Int)

typeCheck :: (Program Liner) -> IO()
typeCheck tree@(Program _ topDefs) = case getFunEnv tree of
    Bad err -> putErrorInStderr err
    Ok fenv -> runCheckFunction fenv topDefs

-- at first we have to create environment for functions
getFunEnv :: (Program Liner) -> Err (FEnv Liner)
getFunEnv (Program _ topDefs) = getFunctionsDef topDefs

getFunctionsDef :: [(TopDef Liner)] -> Err (FEnv Liner)
getFunctionsDef topDefs = do
    fenv <- foldM (updateFun) emptyFEnv topDefs
    case M.lookup (Ident "main") fenv of
        Nothing -> fail "No function named 'main'"
        Just ([], Int _) -> return fenv
        _ -> fail "Bad signature for 'main' function"

-- -- predifined functions added to env during init
emptyFEnv :: (FEnv Liner)
emptyFEnv = M.fromList [
        (Ident "printInt", ([Int Nothing], Void Nothing))]
        {-(Ident "printString", ([Str 0], Void 0)),
        (Ident "error", ([], Void 0)),
        (Ident "readInt", ([], Int 0)),
        (Ident "readString", ([], Str 0))] -}

updateFun :: (FEnv Liner) -> (TopDef Liner) -> Err (FEnv Liner)
updateFun fenv f@(FnDef _ t id@(Ident s) args _) = case M.lookup id fenv of
    Nothing -> return $ (M.insert id ( types args, t) fenv)
    Just _ -> fail $ "Function with name '" ++ s ++ "' was already declared"
    where 
        types _args = map (\(Arg _ t id) -> t) _args

-- -- secondly we can check body of every single function
-- runCheckFunction :: (FEnv a) -> [(TopDef a)] -> IO()
-- runCheckFunction _ [] = putStrLn ("Ok\n")
-- runCheckFunction fenv (h:t) = do
--     r <- evalStateT (runErrorT (runReaderT (checkFunction h) fenv)) []
--     case r of
--         Left err -> putErrorInStderr err
--         Right _ -> runCheckFunction fenv t
  
-- checkFunction :: (TopDef a) -> Mem () a
-- checkFunction f@(FnDef _ t id@(Ident s) args (Block _ stmts)) = do
--     store <- foldM (addArg s) M.empty args
--     put [store]
--     returnsProperly <- foldM (checkStmt t) (t==Void) stmts
--     case returnsProperly of
--         True -> return ()
--         False -> fail $ "Function " ++ s ++ " should return parameter " ++ (show t)

-- addArg :: String -> (VEnv a) -> (Arg a) -> Mem (VEnv a) a
-- addArg fun_id m (Arg t id@(Ident id_)) = case (M.lookup id m) of
--     Nothing -> return (M.insert id t m)
--     Just _ -> fail $ "Two arguments with same name '" ++ id_ ++ "' in function " ++ fun_id

-- checkStmt :: (Type a) -> Bool -> (Stmt a) -> Mem Bool a
-- checkStmt _ b Empty = return b
-- checkStmt t b (BStmt (Block block)) = do
--     s <- get
--     put (M.empty:s)
--     was_return <- foldM (checkStmt t) b block
--     return was_return
-- checkStmt _ b (Decl t items) = do
--    forM items (checkItem t)
--    return b
-- checkStmt _ b (Ass id@(Ident i) exp) = do 
--     exp_type <- infer exp
--     s <- get
--     case findTypeOfIdInStack id s of
--         Bad err -> fail $ "Variable " ++ i ++ " was not declared before assignment"
--         Ok id_type -> case (id_type == exp_type) of
--             True -> return b
--             False -> fail $ "Assignment expression to " ++ i ++ "\nExpected type: " ++ 
--                 (show id_type) ++ "\nGot type: " ++ (show exp_type)
-- checkStmt _ b (Incr id@(Ident i)) = do 
--     s <- get
--     case findTypeOfIdInStack id s of    
--         Bad err -> fail $ "Variable " ++ i ++ " was not declared before incrementation"
--         Ok id_type -> case (id_type == Int) of
--             True -> return b
--             False -> fail $ "Incrementing variable " ++ i ++ " which is non Int type" 
-- checkStmt _ b (Decr id@(Ident i)) = do 
--     s <- get
--     case findTypeOfIdInStack id s of    
--         Bad err -> fail $ "Variable " ++ i ++ " was not declared before decrementation"
--         Ok id_type -> case (id_type == Int) of
--             True -> return b
--             False -> fail $ "Decrementing variable " ++ i ++ " which is non Int type" 
-- checkStmt t _ (Ret exp) = do
--     exp_type <- infer exp
--     case (t == exp_type) of
--         True -> return True
--         False -> fail $ "Expected return type " ++ (show t) ++ " reached " ++ (show exp_type)
-- checkStmt t _ VRet =
--     case (t == Void) of
--         True -> return True
--         False -> fail $ "Expected return type " ++ (show t) ++ " reached void"
-- checkStmt t b (Cond expr stmt) = do
--     exp_type <- infer expr
--     case (exp_type == Bool) of
--         False -> fail $ "Non bool expression in if condition"
--         True -> do
--             foldM (checkStmt t) b [stmt]
--             return b 
-- checkStmt t b (CondElse expr stmt1 stmt2) = do 
--     exp_type <- infer expr
--     case (exp_type == Bool) of
--         False -> fail $ "Non bool expression in if-else condition"
--         True -> do
--             b1 <- foldM (checkStmt t) b [stmt1]
--             b2 <- foldM (checkStmt t) b [stmt2]
--             return (b || (b1 && b2))
-- checkStmt t b (While exp stmt) = do
--     exp_type <- infer exp
--     case (exp_type == Bool) of
--         False -> fail $ "Non bool expression in while condition"
--         True -> do
--             foldM (checkStmt t) b [stmt]
--             return b     
-- checkStmt t b (SExp exp) = do
--     exp_type <- infer exp
--     return b

-- findTypeOfIdInStack :: Ident -> (VStore a) -> Err (Type a)
-- findTypeOfIdInStack id [] = fail "id not in store"
-- findTypeOfIdInStack id (h:t) = case M.lookup id h of
--     Nothing -> findTypeOfIdInStack id t
--     Just type_ -> return type_  

-- checkItem :: (Type a) -> (Item a) -> Mem () a
-- checkItem t item@(NoInit ident@(Ident id)) = do
--     s <- get
--     case (findTypeOfIdInStack ident [head s]) of
--         Bad err -> do
--             put ((M.insert ident t (head s)):(tail s)) 
--             return ()
--         Ok _ -> fail $ "Variable " ++ id ++ " was already declared in this scope"
-- checkItem t item@(Init ident@(Ident id) exp) = do
--     s <- get
--     case (findTypeOfIdInStack ident [head s]) of
--         Bad err -> do
--             exp_type <- infer exp
--             case (exp_type == t) of
--                 True -> do
--                     put ((M.insert ident t (head s)):(tail s)) 
--                     return ()
--                 False -> fail $ "Declaring variable " ++ id ++ 
--                     " with wrong type initializer\ngot: " ++ (show exp_type) ++ 
--                     "\nexpected: " ++ show(t)

--         Ok _ -> fail $ "Variable " ++ id ++ " was already declared in this scope"

-- infer :: (Expr a) -> Mem (Type a) a
-- infer ELitTrue = return Bool
-- infer ELitFalse = return Bool
-- infer (EString _) = return Str
-- infer (ELitInt _) = return Int
-- infer (Not exp) = do
--     exp_type <- infer exp
--     case (exp_type == Bool) of
--         True -> return Bool
--         False -> fail $ "Applying '!'' to non bool expression"
-- infer (Neg exp) = do
--     exp_type <- infer exp
--     case (exp_type == Int) of
--         True -> return Int
--         False -> fail $ "Negating non int expression"
-- infer (EVar ident@(Ident i)) = do
--     s <- get
--     case findTypeOfIdInStack ident s of    
--         Bad err -> fail $ "Variable " ++ i ++ " was not declared but used in expression"
--         Ok id_type -> return id_type
-- infer (EApp ident@(Ident i) exps) = do
--     (res) <- asks (M.lookup ident)
--     case res of 
--         Nothing -> fail $ "Function named " ++ i ++  " was not declared but used in expression"
--         Just (types, ret_typ) -> case (length types) == (length exps) of
--             False -> fail $ "Wrong number of parameters in function " ++ i ++ " application "
--             True -> do
--                 checkFunctionArgs types exps i
--                 return ret_typ
-- infer (EMul exp1 _ exp2) = do
--     type1 <- infer exp1
--     type2 <- infer exp2
--     case (type1 == Int && type2 == Int) of
--         False -> fail $ "Both left and right expression has to be int in mul exp"
--         True -> return Int        
-- infer (EAdd exp1 Plus exp2) = do
--     type1 <- infer exp1
--     type2 <- infer exp2
--     case (type1 == Int && type2 == Int || type1 == Str && type2 == Str) of
--         False -> fail $ "Both left and right expression has to be int or string in add exp"
--         True -> return type1
-- infer (EAdd exp1 Minus exp2) = do
--     type1 <- infer exp1
--     type2 <- infer exp2
--     case (type1 == Int && type2 == Int) of
--         False -> fail $ "Both left and right expression has to be int in minus exp"
--         True -> return Int
-- infer (EAnd exp1 exp2) = do
--     type1 <- infer exp1
--     case (type1 == Bool) of
--         False -> fail "Left side of expression has to be Bool in and-exp"
--         True -> do 
--             type2 <- infer exp1
--             case (type2 == Bool) of
--                 False -> fail "Right side of expression has to be Bool in and-exp"
--                 True -> do return Bool 
-- infer (EOr exp1 exp2) = do
--     type1 <- infer exp1
--     type2 <- infer exp2
--     case (type1 == Bool && type2 == Bool) of
--         False -> fail $ "Both left and right expression has to be bool in or-exp"
--         True -> return Bool
-- infer (ERel expr1 _ expr2) = do    
--     type1 <- infer expr1
--     type2 <- infer expr2
--     case (type1 == type2) of
--         False -> fail $ "Both left and right expression has to be same relop-exp"
--         True -> return Bool

-- checkFunctionArgs:: [(Type a)] -> [(Expr a)] -> String -> Mem () a
-- checkFunctionArgs [] [] _ = return ()
-- checkFunctionArgs (h:t) (x:s) str = do
--     app_type <- infer x
--     case (h==app_type) of
--         True -> checkFunctionArgs t s str
--         False -> fail $ "Wrong type parameter in function " ++ str ++ "application" ++
--             "\nExpected type: " ++ (show h) ++ "\nGot: " ++ show(app_type)