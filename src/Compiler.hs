-- author: Piotr Zieliński

module Main where

import System.Environment
import ErrM
import Control.Monad

import ParLatte
import AbsLatte

import LlvmGenerator
import TypeChecker
import Utils

main :: IO ()
main = do
    arg <- getArgs
    x <- case arg of 
        (f:_) -> readFile f
        otherwise -> getContents
    compile x

compile :: String -> IO ()
compile s = case pProgram (myLexer s) of
    Bad err -> throwMyError err
    Ok tree -> do
        fenv <- typeCheck tree
        generatedCode <- generateCode fenv tree
        putStr generatedCode

generateCode :: (FEnv Liner) -> (Program Liner) -> IO(String)
generateCode fenv (Program _ topDefs) =
    foldM (generateFunctions fenv) initialFunDeclarations topDefs