module Utils (
    throwMyError, throwMySuccess, isInt, isVoid, isBool, isStr, 
    sameType, addLine, showMy, fromType, errNoMain, errBadSignature,
    errFunctionExists, errFunctionShouldReturn, errArgsWithSameName,
    errVarNotDecl, errAssType, errActionBadType, errExpectedReturnType,
    errNonBoolIn, errVarAlreadyDecl, errDeclInitializer, errWrongParType,
    errWrongNumberPar, errFunNotDecl
) where

import System.Exit
import System.IO
import ErrM
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Map as M

import AbsLatte

type Liner = Maybe (Int, Int)
type VStore a = [(VEnv a)]
type FEnv a = M.Map Ident ([(Type a)], (Type a))
type VEnv a = M.Map Ident (Type a)
type Mem a b = ReaderT (FEnv b) (ErrorT String (StateT (VStore b) IO)) a

-- error handling
throwMyError err = do
    hPutStrLn stderr ("ERROR\n" ++ err)
    exitFailure

addLine (Just (a,b)) = "\nError found in " ++ show(a) ++ ":" ++ show(b)  
addLine Nothing = ""

throwMySuccess = hPutStrLn stderr ("OK") 

-- functions returning err messages
errNoMain :: Err (FEnv Liner)
errNoMain = fail "Function 'main' not found"

errBadSignature :: Err (FEnv Liner)
errBadSignature = fail "Bad signature for 'main' function"

errFunctionExists :: String -> Liner -> Err (FEnv Liner)
errFunctionExists s line = fail $ "Function with name '" ++ s ++ 
    "' was already declared" ++ (addLine line)

errFunctionShouldReturn :: String -> (Type Liner) -> Liner -> Mem () Liner 
errFunctionShouldReturn s t line = fail $ "Function '" ++ s ++ 
    "' should return parameter " ++ (showMy t) ++ (addLine line)    

errArgsWithSameName :: String -> String -> Liner -> Mem (VEnv Liner) Liner
errArgsWithSameName id_ fun_id line = fail $ "Two arguments with same name '" ++ id_ ++ 
    "' in function '" ++ fun_id ++ "'" ++ (addLine line)

errVarNotDecl :: String -> String-> Liner -> Mem Bool Liner
errVarNotDecl i action line = fail $ "Variable with id '" ++ i ++ 
    "' was not declared before " ++ action ++ (addLine line)

errAssType :: String -> (Type Liner) -> (Type Liner) -> Liner -> Mem Bool Liner
errAssType i id_type exp_type line = fail $ "Assignment expression with wrong type to variable '" 
    ++ i ++ "'\nExpected type: " ++ (showMy id_type) ++ "\nGot type: " ++ 
    (showMy exp_type) ++ (addLine line)

errActionBadType :: String -> String -> Liner -> Mem Bool Liner
errActionBadType action i line = fail $ action ++ " variable '" ++ i ++ 
    "'' which is non Int type" ++ (addLine line)

errExpectedReturnType :: (Type Liner) -> String -> Liner -> Mem Bool Liner
errExpectedReturnType t s line= fail $ "Expected return type: " ++ (showMy t) 
    ++ "\nReached: " ++ s ++ (addLine line)

errNonBoolIn :: String -> Liner -> Mem Bool Liner
errNonBoolIn in_ line = fail $ "Non bool expression in " ++ in_ ++ " condition" 
    ++ (addLine line)

errVarAlreadyDecl :: String -> Liner -> Mem () Liner
errVarAlreadyDecl id line = fail $ "Variable '" ++ id ++ 
    "' was already declared in this scope" ++ (addLine line)

errDeclInitializer :: String -> (Type Liner) -> (Type Liner) -> Liner -> Mem () Liner
errDeclInitializer id t exp_type line = fail $ "Declaring variable '" ++ id ++ 
    "' with wrong initializer type" ++ "\nExpected type: " ++ showMy(t)
    ++ "\nGot type: " ++ (showMy exp_type) ++ (addLine line) 

errWrongParType :: String -> (Type Liner) -> (Type Liner) -> Liner -> Mem () Liner
errWrongParType str h app_type line = fail $ "Wrong parameter type in function '" 
    ++ str ++ "' application" ++ "\nExpected type: " ++ (showMy h) ++ 
    "\nGot type: " ++ showMy(app_type) ++ (addLine line)

errWrongNumberPar :: String -> Liner -> Mem (Type Liner) Liner
errWrongNumberPar i line = fail $ "Wrong number of parameters in function '" 
    ++ i ++ "' application " ++ (addLine line)

errFunNotDecl :: String -> Liner -> Mem (Type Liner) Liner
errFunNotDecl i line = fail $ "Function named '" ++ i ++  
    "' was not declared but used in expression" ++ (addLine line)

-- needed after adding line error to typechecker
isInt (Int _) = True
isInt _ = False

isVoid (Void _) = True
isVoid _ = False

isBool (Bool _) = True
isBool _ = False

isStr (Str _) = True
isStr _ = False

sameType (Int _) (Int _) = True
sameType (Str _) (Str _) = True
sameType (Void _) (Void _) = True
sameType (Bool _) (Bool _) = True
sameType _ _ = False

showMy (Int _) = "Int"
showMy (Str _) = "Str"
showMy (Void _) = "Void"
showMy (Bool _) = "Bool"
showMy _ = "Unknown type"

fromType (Int l) = l
fromType (Str l) = l
fromType (Void l) = l
fromType (Bool l) = l