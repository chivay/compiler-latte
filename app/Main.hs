module Main where

import System.Environment
import System.IO
import qualified Parse as P

main :: IO ()
main = do
    [filename] <- getArgs
    putStrLn "Latte Compiler v0.1"
    res <- P.parseSource filename
    case res of
        Left err -> print err
        Right program -> print program 
