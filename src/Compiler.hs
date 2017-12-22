module Main where

import System.Environment
import ErrM

import ParLatte
-- import AbsLatte

import TypeChecker
import Utils


compile :: String -> IO ()
compile s = case pProgram (myLexer s) of
    Bad err -> throwError err
    Ok tree -> case typeCheck tree of
        Bad err -> do throwError err
        Ok _ -> putStrLn "OK"

main :: IO ()
main = do
    arg <- getArgs
    x <- case arg of 
        (f:_) -> do readFile f
        otherwise -> do getContents
    compile x