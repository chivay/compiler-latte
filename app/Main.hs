module Main where

import qualified Parse                          as P
import           System.Environment
import           System.IO
import           System.Exit

import           Prettify
import           Text.PrettyPrint.HughesPJClass


failWithError :: String -> IO ()
failWithError err = do
    hPutStrLn stderr err
    exitWith $ ExitFailure 1


compileFile :: String -> IO ()
compileFile filename = do
  res <- P.parseSource filename
  case res of
    Left err      -> print err
    Right program -> print $ pPrint program

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compileFile filename
    _ -> failWithError "Invalid arguments!"
--  putStrLn "Latte Compiler v0.1"
