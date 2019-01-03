{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import           Abs                  as AST
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text            as T
import qualified Data.Map             as M

data CompileError = TypeError T.Text
                  | RedefinitionError
                  deriving Show


type CompilerM = ExceptT CompileError (StateT CompilerState IO)

data CompilerState = CompilerState
  { _filename :: FilePath
  , _src      :: T.Text
  , _ast      :: AST.Program
  , _topDefs  :: M.Map Ident Type
  } deriving (Eq, Show)


removeIdents :: [AST.TypVar] -> [AST.Type]
removeIdents = fmap unpack
    where unpack (AST.TypVar t _ ) = t

loadTopDefinitions :: CompilerM ()
loadTopDefinitions = do
    (AST.Program tds) <- gets _ast
    forM_ tds addDef
    return ()
    where addDef :: TopDef -> CompilerM ()
          addDef (TopDef rtype ident args _ ) = do
                tds <- gets _topDefs
                if M.member ident tds
                   then throwError RedefinitionError
                   else modify insertFunc
                where ftype = AST.TFunc rtype (removeIdents args)
                      insertFunc s = s { _topDefs = M.insert ident ftype (_topDefs s) }

compileProgram :: AST.Program -> IO ()
compileProgram prog = do
    (e, s) <- runStateT (runExceptT compile) initialState
    case e of
      Left e -> putStrLn $ show e
      _ -> putStrLn $ show s
    where compile = do
            loadTopDefinitions
          initialState = CompilerState { _filename = "prog.lat"
                                       , _src = "somethin"
                                       , _ast = prog
                                       , _topDefs = M.empty}

