module Main where

import qualified Parse                          as P
import qualified Compiler                       as C
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
    Left err      -> (failWithError . show) err
    Right program -> do
        (print . pPrint) program
        (putStrLn . show) program
        C.compileProgram program

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compileFile filename
    _ -> failWithError "Invalid arguments!"
--  putStrLn "Latte Compiler v0.1"
