module Main where

import System.Environment
import ErrM

import ParLatte

import TypeChecker
import Utils


compile :: String -> IO ()
compile s = case pProgram (myLexer s) of
    Bad err -> throwMyError err
    Ok tree -> typeCheck tree


main :: IO ()
main = do
    arg <- getArgs
    x <- case arg of 
        (f:_) -> readFile f
        otherwise -> getContents
    compile x