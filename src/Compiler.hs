-- author: Piotr Zieliński

module Main where

import System.Environment
import ErrM

import ParLatte

import LlvmGenerator
import TypeChecker
import AbsLatte
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
        typeCheck tree
        generateCode tree

generateCode :: (Program Liner) -> IO()
generateCode (Program _ topDefs) = do 
    putStr ("; Ok\n")
    putStr $ foldl generateFunctions initialFunDeclarations topDefs