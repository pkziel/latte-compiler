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
import System.Exit
import System.IO

import AbsLatte
import Utils

type VStore = [VEnv]                            -- functions and context stack
type FEnv = M.Map Ident ([Type], Type)          -- function type signature
type VEnv = M.Map Ident Type                    -- variables with their types
type Mem a = ReaderT FEnv (ErrorT String (StateT VStore IO)) a

-- main function invoked by Compiler
typeCheck :: Program -> IO()
typeCheck tree@(Program topDefs) =
    case getFunEnv tree of
        Bad err -> putErrorInStderr err
        Ok fenv -> runCheckFunction fenv topDefs

-- at first we have to create environment for functions
getFunEnv :: Program -> Err FEnv
getFunEnv (Program topDefs) = getFunctionsDef topDefs

getFunctionsDef :: [TopDef] -> Err FEnv
getFunctionsDef topDefs = do
    fenv <- foldM (updateFun) emptyFEnv topDefs
    case M.lookup (Ident "main") fenv of
        Nothing -> fail "No function named 'main'"
        Just ([], Int) -> return fenv
        _ -> fail $ "Bad signature for 'main' function"

-- predifined functions added to env during init
emptyFEnv :: FEnv
emptyFEnv = M.fromList [
        (Ident "printInt", ([Int], Void)),
        (Ident "printString", ([Str], Void)),
        (Ident "error", ([], Void)),
        (Ident "readInt", ([], Int)),
        (Ident "readString", ([], Str))]

updateFun :: FEnv -> TopDef -> Err FEnv
updateFun fenv f@(FnDef t id@(Ident s) args _) = case M.lookup id fenv of
    Nothing -> return $ (M.insert id ( types args, t) fenv)
    Just _ -> fail $ "Function with name '" ++ s ++ "' was already declared"
    where 
        types _args = map (\(Arg t id) -> t) _args

-- secondly we can check body of every single function
runCheckFunction :: FEnv -> [TopDef] -> IO()
runCheckFunction _ [] = return ()
runCheckFunction fenv (h:t) = do
    r <- evalStateT (runErrorT (runReaderT (checkFunction h) fenv)) []
    case r of
        Left err -> putErrorInStderr err
        Right _ -> runCheckFunction fenv t
  
checkFunction :: TopDef -> Mem ()
checkFunction f@(FnDef t id@(Ident s) args block) = do
    store <- foldM (addArg s) M.empty args
    return ()

addArg :: String -> VEnv -> Arg -> Mem VEnv
addArg fun_id m (Arg t id@(Ident id_)) = case (M.lookup id m) of
    Nothing -> return (M.insert id t m)
    Just _ -> fail $ "Two arguments with same name '" ++ id_ ++ "' in function " ++ fun_id