module Utils (
    throwError
) where

import System.Exit
import System.IO

throwError :: String -> IO()
throwError err = do
    hPutStrLn stderr ("ERROR\n" ++ err)
    exitFailure