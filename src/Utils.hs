module Utils (
    throwMyError, putErrorInStderr
) where

import System.Exit
import System.IO

throwMyError err = do
    hPutStrLn stderr ("ERROR\n" ++ err)
    exitFailure

putErrorInStderr err = hPutStrLn stderr ("ERROR\n" ++ err)