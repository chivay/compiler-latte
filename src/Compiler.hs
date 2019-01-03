{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import           Abs                  as AST
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Text            as T

data CompileError
  = TypeError T.Text
  | RedefinitionError
  deriving (Show)

type CompilerM = ExceptT CompileError (StateT CompilerState IO)

data CompilerState = CompilerState
  { _filename :: FilePath
  , _src      :: T.Text
  , _ast      :: AST.Program
  , _topDefs  :: M.Map Ident Type
  } deriving (Eq, Show)


type ScopeLevel = Integer

data TypecheckEnv = TypecheckEnv
  {
  , _topDefs :: M.Map Ident Type
  , _vars :: M.Map Ident (Type, ScopeLevel)
  , _currentScope :: ScopeLevel
  } deriving (Eq, Show)

removeIdents :: [AST.TypVar] -> [AST.Type]
removeIdents = fmap unpack
  where
    unpack (AST.TypVar t _) = t

loadTopDefinitions :: CompilerM ()
loadTopDefinitions = do
  (AST.Program tds) <- gets _ast
  forM_ tds addDef
  where
    addDef :: TopDef -> CompilerM ()
    addDef (TopDef rtype ident args _) = do
      tds <- gets _topDefs
      if M.member ident tds
        then throwError RedefinitionError
        else modify insertFunc
      where
        ftype = AST.TFunc rtype (removeIdents args)
        insertFunc s = s {_topDefs = M.insert ident ftype (_topDefs s)}

checkFunctions :: CompilerM ()
checkFunctions = do
  (AST.Program tds) <- gets _ast
  forM_ tds checkFunction
  return ()
  where checkFunction td = do
          run
          return ()

compileProgram :: AST.Program -> IO ()
compileProgram prog = do
  (e, s) <- runStateT (runExceptT compile) initialState
  case e of
    Left e -> print e
    _      -> print s
  where
    compile = do
      loadTopDefinitions
      checkFunctions
    initialState =
      CompilerState
        { _filename = "prog.lat"
        , _src = "somethin"
        , _ast = prog
        , _topDefs = initialDefinitions
        }
    initialDefinitions =
      M.fromList
        [ ("printInt", func void [int])
        , ("printString", func void [string])
        , ("error", func void [])
        , ("readInt", func int [])
        , ("readString", func string [])
        ]
      where
        void = AST.TVoid
        int = AST.TInteger
        func = AST.TFunc
        string = AST.TString
