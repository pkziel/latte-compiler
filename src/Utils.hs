module Utils (
    throwMyError, putErrorInStderr, Liner, isInt, isVoid, isBool, isStr, 
    sameType, addLine, showMy
) where

import System.Exit
import System.IO

import AbsLatte

-- error handling
throwMyError err = do
    hPutStrLn stderr ("ERROR\n" ++ err)
    exitFailure

putErrorInStderr err = hPutStrLn stderr ("ERROR\n" ++ err)

addLine (Just (a,b)) = "\nError found in " ++ show(a) ++ ":" ++ show(b)  
addLine Nothing = ""

-- needed after adding line error to typechecker
type Liner = Maybe (Int, Int)

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