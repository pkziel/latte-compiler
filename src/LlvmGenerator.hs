module LlvmGenerator (
    generateFunctions
) where

import qualified Data.Map as M
import System.IO
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import Utils

-- type ConstMap = M.Map String String
type VarEnv a = M.Map Ident ((Type a), String)
type VarStore a = [VarEnv a]
type StringStore = M.Map String String
-- in state we store counter for registers, variable stack, string store
type Code a b = ReaderT (FEnv b) (StateT (Int, (VarStore b), StringStore) IO) a

generateFunctions :: (FEnv Liner) -> (StringStore, Int, String) -> (TopDef Liner) -> IO(StringStore, Int, String)
generateFunctions fenv (accM, accC, acc) (FnDef _ t (Ident id) args block) = do
    (body, (count ,_, consts)) <- runStateT (runReaderT (generateFun args block) fenv) 
        (initialStore accC accM)
    return (consts, count, acc ++ "define " ++ (printType t) ++ " @" ++ 
        id ++ "(" ++ (printArgsInFun args) ++ "){\n" ++ body ++ (maybeRet t) ++ "}\n\n")
maybeRet t = case t of
    Void _ -> "\tret void\n"
    -- this is in case of empty label at the end (llvm doesn't accept it)
    -- typechecker should provide that this will not be used
    _ -> "\tret "++ printType t ++ " undef\n" 
initialStore counter map_ = (counter, [M.empty], map_)

generateFun :: [Arg Liner] -> (Block Liner) -> Code String Liner
generateFun args (Block _ stmts) = do
    s1 <- foldM generateArgAlloca "" args
    s2 <- foldM generateStmts "" stmts
    return $ s1 ++ s2

generateArgAlloca :: String -> (Arg Liner) -> Code String Liner
generateArgAlloca acc (Arg _ t i@(Ident id)) = do
    reg1 <- giveNewVarRegister
    insertNewVariable i t reg1
    return $ acc ++ printAlloca reg1 t ++ printStore t ("%" ++ id) reg1

takeNewRegister :: Code Int Liner
takeNewRegister = do
    (x, v, z) <- get
    put (x+1, v, z) 
    return x

giveNewVarRegister :: Code String Liner
giveNewVarRegister = do
    reg <- takeNewRegister
    return $ "%reg_" ++ show reg

giveNewLabel :: Code String Liner
giveNewLabel = do
    reg <- takeNewRegister
    return $ "label" ++ show reg

addNewStringConstant :: String -> Code String Liner
addNewStringConstant s = do
    (x, v, m) <- get
    case M.lookup s m of
        Nothing -> do
            newRegNr <- takeNewRegister
            put (x, v, M.insert s (".str" ++ show newRegNr) m)
            return (".str" ++ show newRegNr)
        Just reg -> return reg

insertNewVariable :: Ident -> (Type Liner) -> String ->  Code () Liner
insertNewVariable id t reg = do
    (p, s, v) <- get
    put (p, (M.insert id (t, reg) (head s):(tail s)), v)

generateStmts :: String -> (Stmt Liner)-> Code String Liner
generateStmts s1 x = do
    s2 <- generateStmt x
    return $ s1 ++ s2 

generateStmt :: (Stmt Liner) -> Code String Liner
generateStmt (Empty _) = return ""
generateStmt (BStmt _ (Block _ stmts)) = do
    (p, s, v) <- get
    put (p, M.empty:s, v)
    res <- foldM generateStmts "" stmts
    modify (\(shouldStay, _, shouldStay2) -> (shouldStay, s, shouldStay2))
    return res
generateStmt (Decl _ t items) = foldM (generateDeclVar t) "" items
generateStmt (Ass _ i@(Ident id) exp) = do
    (type_, register) <- findVar i
    (code, _, res) <- generateExpr exp
    return $ printStore type_ res register    
generateStmt (Incr _ i) = generateStmt (Ass Nothing i (EAdd Nothing 
    (EVar Nothing i) (Plus Nothing) (ELitInt Nothing 1)))
generateStmt (Decr _ i) = generateStmt (Ass Nothing i (EAdd Nothing 
    (EVar Nothing i) (Minus Nothing) (ELitInt Nothing 1)))
generateStmt (Ret _ exp) = do
    (code, type_, val) <- generateExpr exp
    return $ code ++ "\tret " ++ printType type_ ++ " " ++ val ++ "\n"    
generateStmt (VRet _) = return "\tret void\n"
generateStmt (Cond _ exp stmt) = do
    (code, type_, val) <- generateExpr exp
    ifLabel <- giveNewLabel
    afterIfLabel <- giveNewLabel
    brReg <- giveNewVarRegister
    inIf <- generateStmt stmt
    return $ code ++ printIf ifLabel afterIfLabel brReg val inIf
generateStmt (CondElse _ exp stmt1 stmt2) = do
    (code, type_, val) <- generateExpr exp
    ifLabel <- giveNewLabel
    elseLabel <- giveNewLabel
    afterLabel <- giveNewLabel
    brReg <- giveNewVarRegister
    inIf <- generateStmt stmt1
    inElse <- generateStmt stmt1
    return $ code ++ printIfElse ifLabel elseLabel afterLabel brReg val inIf inElse
generateStmt (While _ exp stmt) = do
    (code, type_, val) <- generateExpr exp
    conditionLabel <- giveNewLabel
    whileLabel <- giveNewLabel
    afterLabel <- giveNewLabel
    brReg <- giveNewVarRegister
    inWhile <- generateStmt stmt
    return $ printWhile code conditionLabel whileLabel afterLabel brReg val inWhile
generateStmt (SExp _ exp) = do
    (code, type_, val) <- generateExpr exp
    return code

generateDeclVar :: (Type Liner) -> String -> (Item Liner) -> Code String Liner
generateDeclVar t@(Str _) v (NoInit _ i@(Ident id)) = do
    reg1 <- giveNewVarRegister
    regWithString <- addNewStringConstant ""
    return $ v ++ printAlloca reg1 t ++ printBitcast reg1 "xx" regWithString
generateDeclVar t v (NoInit _ i@(Ident id)) = do
    reg1 <- giveNewVarRegister
    insertNewVariable i t reg1
    return $ v ++ printAlloca reg1 t ++ printStore t (giveInitialValue t) reg1
generateDeclVar t v (Init _ i@(Ident id) exp) = do
    (code, _, res) <- generateExpr exp
    reg1 <- giveNewVarRegister
    insertNewVariable i t reg1
    return $ code ++ v ++ printAlloca reg1 t ++ printStore t res reg1

findVar :: Ident -> Code ((Type Liner), String) Liner
findVar i = do
    (p, s, v) <- get
    findRegInStack i s

findRegInStack :: Ident -> (VarStore Liner) -> Code ((Type Liner), String) Liner
findRegInStack id (h:t) = case M.lookup id h of
    Nothing -> findRegInStack id t
    Just sth -> return sth  

-- we take expr, label true, label left
-- we return code generated by expr, type of result and register/value
generateExpr :: (Expr Liner) -> Code (String, (Type Liner), String) Liner
generateExpr (EVar _ id) = do
    (type_, register) <- findVar id
    newReg <- giveNewVarRegister
    return (printLoad newReg type_ register , type_, newReg)
generateExpr (ELitInt _ num) = return ("", (Int Nothing), show num)
generateExpr (ELitTrue _) = return ("", (Bool Nothing), "true")
generateExpr (ELitFalse _) = return ("", (Bool Nothing), "false")
generateExpr (EApp _ ident@(Ident i) exprs) = do
    (res) <- asks (M.lookup ident)
    case res of 
        Just (types, ret_typ) -> do
            (preCode, inCode) <- foldM generateCallArgs ("", []) exprs
            newRegister <- takeNewRegister
            return (preCode ++ printCall ret_typ newRegister i inCode, ret_typ, 
                "%" ++ show newRegister)            
generateExpr (EString _ str) = do
    reg1 <- giveNewVarRegister
    regWithString <- addNewStringConstant str
    return (printBitcast reg1 str ("@" ++ regWithString), (Str Nothing), reg1)
generateExpr (Neg _ expr) = generateExpr (EMul Nothing (ELitInt Nothing (-1)) (Times Nothing) expr)
generateExpr (Not _ expr) = do
    (code , _, reg2) <- generateExpr expr
    reg1 <- giveNewVarRegister
    return (code ++ printXor reg1 reg2 , (Bool Nothing), reg1)

generateCallArgs :: (String, [((Type Liner), String)]) -> (Expr Liner) -> 
    Code (String, [((Type Liner), String)]) Liner
generateCallArgs (acc1, acc2) exp = do
    (code, type_, res) <- generateExpr exp
    return (acc1 ++ code, acc2 ++ [(type_, res)])

