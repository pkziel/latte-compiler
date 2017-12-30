module LlvmGenerator (
    initialFunDeclarations, generateFunctions
) where

import AbsLatte

type Liner = Maybe (Int, Int)

initialFunDeclarations :: String
initialFunDeclarations =
    "declare void @printInt(i32)\n" ++ 
    "declare void @printString(i8*)\n" ++ 
    "declare void @error()\n" ++
    "declare i32 @readInt()\n" ++ 
    "declare i8* @readString()\n" ++ 
    "declare i8* @concat(i8*, i8*)\n\n"


generateFunctions :: String -> (TopDef Liner) -> String
generateFunctions acc (FnDef _ t (Ident id) args block) =
    acc ++ "define " ++ (printType t) ++ " @" ++ id ++ "(" ++ (printArgsInFun args) ++ "){\n" ++ "}\n"


printType :: (Type Liner) -> String
printType (Void _) = "void"
printType (Str _) = "i8*" 
printType (Int _) = "i32"
printType (Bool _) = "i1"

printArgsInFun :: [(Arg Liner)] -> String
printArgsInFun [] = ""
printArgsInFun ((Arg _ t (Ident id)):[]) = printType t ++ " %" ++  id
printArgsInFun ((Arg _ t (Ident id)):y) = printType t ++ " %" ++  id ++ ", "++ printArgsInFun y