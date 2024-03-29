module Main where

import qualified Parse                          as P
import qualified Compiler                       as C
import           System.Environment
import           System.IO
import           System.Exit
import Prettify
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

failWithError :: String -> IO ()
failWithError err = do
    hPutStrLn stderr err
    exitWith $ ExitFailure 1

compileFile :: String -> IO ()
compileFile filename = do
  res <- P.parseSource filename
  case res of
    Left err      -> do
        hPutStrLn stderr "ERROR"
        (failWithError . show) err
    Right program -> do
        hPutStrLn stderr $ show  program
        hPutStrLn stderr $ show $ pPrint program
        C.compileProgram program
        hPutStrLn stderr "OK"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> compileFile filename
    _ -> failWithError "Invalid arguments!"
