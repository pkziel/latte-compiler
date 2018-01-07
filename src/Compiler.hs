-- author: Piotr Zieliński

module Main where

import System.Environment
import ErrM
import Control.Monad
import qualified Data.Map as M

import ParLatte
import AbsLatte

import LlvmGenerator
import TypeChecker
import Utils

type StringStore = M.Map String String

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
        (consts, _, generatedCode) <- generateCode fenv tree
        putStr $ (printConsts consts) ++ "\n" ++ generatedCode

generateCode :: (FEnv Liner) -> (Program Liner) -> IO(StringStore, Int, String)
generateCode fenv (Program _ topDefs) = do
    foldM (generateFunctions fenv) (M.empty, 1, initialFunDeclarations) topDefs