module Main where

import qualified Parse                          as P
import           System.Environment
import           System.IO

import           Prettify
import           Text.PrettyPrint.HughesPJClass

main :: IO ()
main = do
  [filename] <- getArgs
  putStrLn "Latte Compiler v0.1"
  res <- P.parseSource filename
  case res of
    Left err      -> print err
    Right program -> print $ pPrint program
