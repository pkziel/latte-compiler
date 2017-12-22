module TypeChecker (
    typeCheck
) where

import ErrM
import qualified Data.Map as M
import Control.Monad  
import System.Exit
import System.IO

import AbsLatte
import Utils


type Env = (FEnv, [VEnv])               -- functions and context stack
type FEnv = M.Map Ident ([Type], Type)  -- function type signature
type VEnv = M.Map Ident Type            -- variables with their types

typeCheck :: Program -> Err FEnv
typeCheck (Program topDefs) = getFunctionsDef topDefs

getFunctionsDef :: [TopDef] -> Err FEnv
getFunctionsDef topDefs = do
    fenv <- foldM (updateFun) emptyFEnv topDefs
    case M.lookup (Ident "main") fenv of
        Nothing -> fail $ "Program has to contain function named 'main'"
        Just ([], Int) -> return fenv
        _ -> fail $ "Bad signature for 'main' function"

-- predifined functions added to env on start
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
    Just _ -> fail $ "Function with name '" ++ s ++ "' was already declared."
    where 
        types _args = map (\(Arg t id) -> t) _args