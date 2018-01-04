module LlvmGenerator (
    generateFunctions
) where

import qualified Data.Map as M
import System.IO
import Control.Monad.State
import Control.Monad.Reader

import AbsLatte
import Utils

type VarEnv a = M.Map Ident ((Type a), String)
type VarStore a = [VarEnv a]
type CounterState = (Int, Int) -- counter for registers, string constants
type Code a b = ReaderT (FEnv b) (StateT (CounterState, (VarStore b)) IO) a

generateFunctions :: (FEnv Liner) -> String ->  (TopDef Liner) -> IO(String) 
generateFunctions fenv acc (FnDef _ t (Ident id) args block) = do
    (consts, body) <- evalStateT (runReaderT (generateFun args block) fenv) initialStore
    return $ consts ++ acc ++ "define " ++ (printType t) ++ " @" ++ id ++ "(" ++ 
        (printArgsInFun args) ++ "){\n" ++ body ++ "}\n"

initialStore = ((1,1), [M.empty])

generateFun :: [Arg Liner] -> (Block Liner) -> Code (String, String) Liner
generateFun args (Block _ stmts) = do
    s1 <- generateArgAlloca args ""
    (consts, s2) <- generateStmts stmts ("", "")
    return (consts, s1 ++ s2)

generateArgAlloca :: [Arg Liner] -> String -> Code String Liner
generateArgAlloca [] acc = return acc
generateArgAlloca ((Arg _ t i@(Ident id)) : y) acc = do
    reg1 <- takeNewRegister
    insertNewVariable i t reg1
    generateArgAlloca y (acc ++ printAlloca reg1 t ++ printStore t id reg1)

takeNewRegister :: Code Int Liner
takeNewRegister = do
    ((x,y), v) <- get
    put (((x+1), y), v) 
    return x

insertNewVariable :: Ident -> (Type Liner) -> Int ->  Code () Liner
insertNewVariable id t nr = do
    (p, s) <- get
    put (p, (M.insert id (t, ("%" ++ show nr)) (head s):(tail s)))

generateStmts :: [Stmt Liner] -> (String, String) -> Code (String, String) Liner
generateStmts [] res = return res
generateStmts (x:s) (c1, s1) = do
    (c2, s2) <- generateStmt x
    generateStmts s (c1++c2, s1++s2)

generateStmt :: (Stmt Liner) -> Code (String, String) Liner
generateStmt (Empty _) = return ("","")
generateStmt (BStmt _ (Block _ stmts)) = do
    (p, s) <- get
    put (p, M.empty:s)
    res <- generateStmts stmts ("","")
    modify (\(shouldStay, _) -> (shouldStay, s))
    return res
generateStmt (Decl _ t items) = do
    foldM (generateDeclVar t) ("","") items


generateDeclVar :: (Type Liner) -> (String, String) -> (Item Liner) -> Code (String, String) Liner
generateDeclVar t (l,r) (NoInit _ (Ident id)) = do

    return (l,r)
generateDeclVar t (l,r) (Init _ (Ident id) exp) = return (l,r)
