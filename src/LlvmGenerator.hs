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
generateFun args block = do
    s1 <- generateArgAlloca args ""
    return ("", s1)

generateArgAlloca :: [Arg Liner] -> String -> Code String Liner
generateArgAlloca [] acc = return acc
generateArgAlloca ((Arg _ t i@(Ident id)) : y) acc = do
    reg1 <- takeNewRegister
    insertNewVariable i t reg1
    generateArgAlloca y (acc ++ "   %" ++ show reg1 ++ " = alloca " ++ (printType t)
        ++ "\n   store " ++ (printType t) ++ " " ++ "%" ++ id ++ ", " ++ (printType t) 
        ++ "*" ++ " %" ++ show reg1 ++ "\n")

takeNewRegister :: Code Int Liner
takeNewRegister = do
    ((x,y), v) <- get
    put (((x+1), y), v) 
    return x

insertNewVariable :: Ident -> (Type Liner) -> Int ->  Code () Liner
insertNewVariable id t nr= do
    (p, s) <- get
    put (p, (M.insert id (t, ("%" ++ show nr)) (head s):(tail s)))

