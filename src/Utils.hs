module Utils (
    --grouped by theme
    throwMyError, throwMySuccess,

    isInt, isVoid, isBool, isStr, sameType, addLine, showMy, fromType, 

    errNoMain, errBadSignature, errFunctionExists, errFunctionShouldReturn, 
    errArgsWithSameName, errVarNotDecl, errAssType, errActionBadType, 
    errExpectedReturnType, errNonBoolIn, errVarAlreadyDecl, errDeclInitializer, 
    errWrongParType, errWrongNumberPar, errFunNotDecl,

    initialFunDeclarations, giveInitialValue,

    printType, printArgsInFun, printAlloca, printStore, printLoad, printCall, 
    printIf, printIfElse, printWhile, printBitcast, printConsts, printXor,
    printLazyAndFir, printLazyAndSec, printPhi, printLazyOrFir, printMul,
    printAddInt, printCallStrConcat, printIntRel,

    Liner, VStore, FEnv, VEnv, Mem
) where

import System.Exit
import System.IO
import ErrM
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Map as M

import AbsLatte

type Liner = Maybe (Int, Int)                     -- place/line in code added to tree
type VStore a = [VEnv a]                          -- context stack
type FEnv a = M.Map Ident ([Type a], (Type a))    -- function type signature
type VEnv a = M.Map Ident (Type a)                -- variables with their types
type Mem a b = ReaderT (FEnv b) (ErrorT String (StateT (VStore b) IO)) a

-- error handling
throwMyError err = do
    hPutStrLn stderr ("ERROR\n" ++ err)
    exitFailure

addLine (Just (a,b)) = "\nError found in " ++ show(a) ++ ":" ++ show(b)  
addLine Nothing = ""

throwMySuccess = hPutStrLn stderr ("OK") 

-- functions returning err messages for typechecker
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

-- functions than simply return String-llvm code / without much haskell logic
-- word print can be a little misplaced
initialFunDeclarations :: String
initialFunDeclarations =
    "declare void @printInt(i32)\n" ++ 
    "declare void @printString(i8*)\n" ++ 
    "declare void @error()\n" ++
    "declare i32 @readInt()\n" ++ 
    "declare i8* @readString()\n" ++ 
    "declare i8* @concat(i8*, i8*)\n\n"

printType :: (Type Liner) -> String
printType (Void _) = "void"
printType (Str _) = "i8*" 
printType (Int _) = "i32"
printType (Bool _) = "i1"

printArgsInFun :: [Arg Liner] -> String
printArgsInFun [] = ""
printArgsInFun ((Arg _ t (Ident id)):[]) = printType t ++ " %" ++  id
printArgsInFun ((Arg _ t (Ident id)):y) = printType t ++ " %" ++  id ++ ", "++ printArgsInFun y

printAlloca :: String -> (Type Liner) -> String
printAlloca i t = "\t" ++ i ++ " = alloca " ++ (printType t) ++ "\n"

printStore :: (Type Liner) -> String -> String -> String
printStore t val reg1 = "\tstore " ++ (printType t) ++ " " ++ val ++ ", " ++ 
    (printType t) ++ "*" ++ " " ++ reg1 ++ "\n"

printBitcast :: String -> String -> String -> String
printBitcast reg lenofstrq val = "\t" ++ reg ++ " = bitcast[" ++ 
    show (length (stringWithoutQ lenofstrq) + 1) ++ " x i8]* " ++ val ++ " to i8*\n"

giveInitialValue :: (Type Liner) -> String
giveInitialValue (Int _) = "0"
giveInitialValue (Bool _) = "false"

printLoad :: String -> (Type Liner) -> String -> String
printLoad newRegister type_ locReg = "\t" ++ newRegister ++ " = load " ++ 
    printType type_ ++ ", " ++ (printType type_) ++ "* " ++ locReg ++ "\n"

printCall :: (Type Liner) -> String -> String -> [((Type Liner), String)] -> String
printCall type_ reg id arr = case type_ of
    Void _ -> "\tcall " ++ printType type_ ++ " @" ++ id ++ "(" ++ printCallArgs arr ++ ")\n" 
    _ -> "\t" ++ reg ++ " = call " ++ printType type_ ++
        " @" ++ id ++ "(" ++ printCallArgs arr ++ ")\n" 

printCallArgs :: [((Type Liner), String)] -> String
printCallArgs [] = ""
printCallArgs ((t, s):[]) = printType t ++ " " ++ s
printCallArgs ((t, s):y) = printType t ++ " " ++  s ++ ", "++ printCallArgs y

printLabel :: String -> String
printLabel label = label ++ ":\n"

printGoto :: String -> String
printGoto label = "\tbr label %" ++ label ++ "\n" 

printBr :: String -> String -> String -> String -> String
printBr brReg val ifLabel afterIfLabel = 
    "\t" ++ brReg ++ " = icmp eq i1 " ++ val ++ ", true\n" ++
    "\tbr i1 " ++ brReg ++ ", label %" ++ ifLabel ++ ", label %" ++ afterIfLabel ++ "\n"

printIf :: String -> String -> String -> String -> String -> String
printIf ifLabel afterIfLabel brReg val inIf =
    printBr brReg val ifLabel afterIfLabel ++ 
    printLabel ifLabel ++ 
    inIf ++ 
    printGoto afterIfLabel ++
    printLabel afterIfLabel

printIfElse :: String -> String -> String -> String -> String -> String -> String -> String
printIfElse ifLabel elseLabel afterLabel brReg val inIf inElse =
    printBr brReg val ifLabel elseLabel ++
    printLabel ifLabel ++
    inIf ++
    printGoto afterLabel ++
    printLabel elseLabel ++
    inElse ++ 
    printGoto afterLabel ++
    printLabel afterLabel

printWhile :: String -> String -> String -> String -> String -> String -> String -> String
printWhile expCode conditionLabel whileLabel afterLabel brReg val inWhile =
    printGoto conditionLabel ++
    printLabel conditionLabel ++
    expCode ++
    printBr brReg val whileLabel afterLabel ++
    printLabel whileLabel ++
    inWhile ++
    printGoto conditionLabel ++
    printLabel afterLabel

printConsts :: M.Map String String -> String
printConsts map_ = M.foldlWithKey addNewConst "" map_

addNewConst :: String -> String -> String -> String 
addNewConst acc value key = acc ++ "@" ++ key ++ " = private constant [" ++ 
    show (length (stringWithoutQ value) + 1) ++ " x i8] c\"" ++ stringWithoutQ value 
    ++ "\\00\"" ++ "\n"

stringWithoutQ :: String -> String
stringWithoutQ str = 
    case (length str > 2) of
        True -> reverse $ tail $ reverse $ tail str
        False -> ""

printXor :: String -> String -> String
printXor res reg1 = "\t" ++ res ++ " = xor i1 true, " ++ reg1 ++ "\n"

printLazyAndFir :: String -> String -> String -> String -> String -> String -> String
printLazyAndFir exp1Label exp1Code resExp1Reg helpReg1 exp2Label afterLabel = 
    printGoto exp1Label ++ 
    printLabel exp1Label ++ 
    exp1Code ++ 
    printBr helpReg1 resExp1Reg exp2Label afterLabel

printLazyAndSec :: String -> String -> String -> String -> String -> String 
printLazyAndSec exp2Label exp2Code resExp2Reg afterLabel middleLabel = 
    printLabel exp2Label ++ 
    exp2Code ++ 
    printGoto middleLabel ++ 
    printLabel middleLabel ++
    printGoto afterLabel ++ 
    printLabel afterLabel

printPhi :: String -> String -> String -> String -> String -> String
printPhi res label1 label2 reg1 reg2 = "\t" ++ res ++ " = phi i1 [" ++ reg1 ++
    ", %" ++ label1 ++ "], [" ++ reg2 ++ ", %" ++ label2 ++ "]\n"

printLazyOrFir :: String -> String -> String -> String -> String -> String -> String
printLazyOrFir exp1Label exp1Code resExp1Reg helpReg1 exp2Label afterLabel = 
    printGoto exp1Label ++ printLabel exp1Label ++ exp1Code ++ 
    printBr helpReg1 resExp1Reg afterLabel exp2Label

printMul :: String -> (MulOp Liner) -> String -> String -> String
printMul resReg mulOp resExp1Reg resExp2Reg = "\t" ++ resReg ++ " = " ++ 
    printMulOp mulOp ++ " i32 " ++ resExp1Reg ++ ", " ++ resExp2Reg ++ "\n"

printMulOp :: (MulOp Liner) -> String 
printMulOp (Times _) = "mul"
printMulOp (Div _) = "sdiv"
printMulOp (Mod _) = "srem"

printAddInt :: String -> (AddOp Liner) -> String -> String -> String
printAddInt resReg addOp resExp1Reg resExp2Reg = "\t" ++ resReg ++ " = " ++ 
    printAddOp addOp ++ " i32 " ++ resExp1Reg ++ ", " ++ resExp2Reg ++ "\n"

printAddOp :: (AddOp Liner) -> String
printAddOp (Plus _) = "add"
printAddOp (Minus _) = "sub"

printCallStrConcat :: String -> String -> String -> String 
printCallStrConcat res arg1 arg2 = "\t" ++ res ++ " = call i8* @concat(i8* " ++
    arg1 ++ ", i8* " ++ arg2 ++ ")\n"

printIntRel :: String -> (RelOp Liner) -> (Type Liner) -> String -> String -> String 
printIntRel res op type_ left right = "\t" ++ res ++ " = icmp " ++ printOp op ++
    " " ++ printType type_ ++ " " ++ left ++ ", " ++ right ++ "\n"

printOp :: (RelOp Liner) -> String
printOp (LTH _) = "slt"
printOp (LE _) = "sle"
printOp (GTH _) = "sgt"
printOp (GE _) = "sge"
printOp (EQU _) = "eq"
printOp (NE _) = "ne"