{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import           Abs                  as AST
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import qualified Data.Map             as M
import qualified Data.Text            as T

data CompileError
  = TypeError T.Text
  | RedefinitionError
  | UndefinedVariableError
  | InvalidTypeError
  deriving (Show)

type CompilerM = ExceptT CompileError (StateT CompilerState IO)

data CompilerState = CompilerState
  { _filename :: FilePath
  , _src      :: T.Text
  , _ast      :: AST.Program
  , _topDefs  :: M.Map Ident Type
  } deriving (Eq, Show)


type TypecheckM = ReaderT TypecheckEnv (Except CompileError)

type ScopeLevel = Integer

data TypecheckEnv = TypecheckEnv
  { _funcDefs     :: M.Map Ident Type
  , _vars         :: M.Map Ident (Type, ScopeLevel)
  , _currentScope :: ScopeLevel
  , _returnType   :: Type
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
  forM_ tds checkDef
  where checkDef :: AST.TopDef -> CompilerM ()
        checkDef (AST.TopDef rtype fname args body) = do
          tds <- gets _topDefs
          let initVars = M.fromList (fmap (\(TypVar typ name) -> (name,(typ,0))) args)
          let initialEnv = TypecheckEnv { _funcDefs = tds
                                        , _vars = initVars
                                        , _currentScope = 0
                                        , _returnType = rtype}
          case runExcept (runReaderT (forM_ body checkStmt) initialEnv) of
            (Left e) -> throwError e
            _ -> return ()
          where checkStmt :: AST.Stmt -> TypecheckM ()
                checkStmt Empty = ok
                checkStmt (Block stmts) = local newScope (forM_ stmts checkStmt)
                checkStmt (Decl typ items) = ok -- TODO
                checkStmt (Ass var expr) = ok -- TODO
                checkStmt (Incr var) = do
                    (typ, _) <- getVar var
                    if typ == AST.TInteger
                       then ok
                       else throwError InvalidTypeError
                checkStmt (Decr var) = do
                    (typ, _) <- getVar var
                    when (typ /= AST.TInteger) (throwError InvalidTypeError)
                checkStmt (Ret Nothing) = do
                    rtyp <- asks _returnType
                    when (rtyp /= AST.TVoid) (throwError InvalidTypeError)
                checkStmt (Ret (Just expr)) = ok -- TODO
                checkStmt (If expr stmt) = ok -- TODO
                checkStmt (IfElse expr stmt stmt') = ok -- TODO
                checkStmt (Loop expr stmt) = ok -- TODO
                checkStmt (ExpS expr) = ok -- TODO


                newScope :: TypecheckEnv -> TypecheckEnv
                newScope env = env { _currentScope = _currentScope env + 1 }

                getVar :: Ident -> TypecheckM (Type, ScopeLevel)
                getVar var = do
                    vars <- asks _vars
                    case M.lookup var vars of
                      Just v -> return v
                      Nothing -> throwError UndefinedVariableError
                ok = return ()



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
