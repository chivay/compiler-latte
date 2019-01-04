{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler where

import           Abs                  as AST
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Applicative
import           Data.Monoid
import qualified Data.Map             as M
import qualified Data.Text            as T

data CompileError
  = TypeError T.Text
  | RedefinitionError
  | UndefinedVariableError AST.Ident
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
type VEnv = M.Map Ident (Type, ScopeLevel)

data TypecheckEnv = TypecheckEnv
  { _funcDefs     :: M.Map Ident Type
  , _vars         :: VEnv
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
          liftIO $ (putStrLn . T.unpack) fname
          tds <- gets _topDefs
          let initVars = M.fromList (fmap (\(TypVar typ name) -> (name,(typ,0))) args)
          when (M.size initVars /= length args) $ throwError RedefinitionError
          let initialEnv = TypecheckEnv { _funcDefs = tds
                                        , _vars = initVars
                                        , _currentScope = 0
                                        , _returnType = rtype}
          case runExcept (runReaderT (checkAll initialEnv body) initialEnv) of
            (Left e) -> throwError e
            _ -> return ()
          where
                runInEnv a e = local (const e) a
                checkAll :: TypecheckEnv -> [AST.Stmt] -> TypecheckM TypecheckEnv
                checkAll env stmts = (foldl (>=>) (const $ return env) (fmap (runInEnv.checkStmt) stmts)) env
                checkStmt :: AST.Stmt -> TypecheckM TypecheckEnv
                checkStmt (Block stmts) = do
                    env <- ask
                    checkAll (newScope env) stmts
                    id
                checkStmt (Decl typ items) = do
                    env <- ask
                    forM_ items checkItemType
                    vars' <- foldM newScopedVar (_vars env) items
                    return $ env {_vars = vars'}
                    where newScopedVar :: VEnv -> DeclItem -> TypecheckM VEnv
                          newScopedVar vars (DeclItem vname initial) = do
                              currentScope <- asks _currentScope
                              case M.lookup vname vars of
                                Just (_, scopeLevel) ->
                                    if currentScope > scopeLevel
                                       then return $ M.insert vname (typ, currentScope) vars
                                       else throwError RedefinitionError
                                Nothing -> return $ M.insert vname (typ, currentScope) vars

                          checkItemType :: DeclItem -> TypecheckM ()
                          checkItemType (DeclItem _ (Just expr)) = do
                            et <- checkExpr expr
                            when (et /= typ) $ throwError InvalidTypeError
                          checkItemType (DeclItem _ _ ) = return () -- default initialized
                checkStmt (Ass var expr) = do
                    (typ, _) <- getVar var
                    typ' <- checkExpr expr
                    when (typ /= typ') $ throwError InvalidTypeError
                    id
                checkStmt (Incr var) = do
                    (typ, _) <- getVar var
                    when (typ /= AST.TInteger) (throwError InvalidTypeError)
                    id
                checkStmt (Decr var) = do
                    (typ, _) <- getVar var
                    when (typ /= AST.TInteger) (throwError InvalidTypeError)
                    id
                checkStmt (Ret Nothing) = do
                    rtyp <- asks _returnType
                    when (rtyp /= AST.TVoid) (throwError InvalidTypeError)
                    id
                checkStmt Empty = id
                checkStmt (Ret (Just expr)) = do
                    rtype <- asks _returnType
                    etype <- checkExpr expr
                    when (rtype /= etype) (throwError InvalidTypeError)
                    id
                checkStmt (If expr stmt) = do
                    etype <- checkExpr expr
                    when (etype /= AST.TBool) (throwError InvalidTypeError)
                    checkStmt stmt
                    id
                checkStmt (IfElse expr stmt stmt') = do
                    etype <- checkExpr expr
                    when (etype /= AST.TBool) (throwError InvalidTypeError)
                    checkStmt stmt
                    checkStmt stmt'
                    id
                checkStmt (Loop expr stmt) = do
                    etype <- checkExpr expr
                    when (etype /= AST.TBool) (throwError InvalidTypeError)
                    checkStmt stmt
                    id
                checkStmt (ExpS expr) = do
                    checkExpr expr
                    id


                checkExpr :: Expr -> TypecheckM Type
                checkExpr (Var vname) = do
                    vars <- asks _vars
                    case M.lookup vname vars of
                      Just (t,_) -> return t
                      Nothing -> throwError $ UndefinedVariableError vname
                checkExpr (LitInt _) = return AST.TInteger
                checkExpr LitTrue = return AST.TBool
                checkExpr LitFalse = return AST.TBool
                checkExpr (Call fname exprs) = do
                    fenv <- asks _funcDefs
                    params <- mapM checkExpr exprs
                    case M.lookup fname fenv of
                      Just (TFunc rt ats) -> do
                          when (params /= ats) $ throwError InvalidTypeError
                          return rt
                      Just _ -> undefined
                      Nothing -> throwError $ UndefinedVariableError fname
                checkExpr (LitString _) = return AST.TString
                checkExpr (Neg expr) = do
                    t <- checkExpr expr
                    when (t /= AST.TInteger) $ throwError InvalidTypeError
                    return AST.TInteger
                checkExpr (Not expr) = do
                    t <- checkExpr expr
                    when (t /= AST.TBool) $ throwError InvalidTypeError
                    return AST.TBool
                checkExpr (Mul _ exp exp') = checkBinOp AST.TInteger AST.TInteger exp exp'
                checkExpr (Add AST.Plus exp exp') = do
                    t <- checkExpr exp
                    t' <- checkExpr exp'
                    when (t /= t') $ throwError InvalidTypeError
                    unless (elem t [AST.TInteger, AST.TString]) $ throwError InvalidTypeError
                    return t
                checkExpr (Add _ exp exp') = checkBinOp AST.TInteger AST.TInteger exp exp'
                checkExpr (Comp _ exp exp') = checkBinOp AST.TBool AST.TInteger exp exp'
                checkExpr (And exp exp') = checkBinOp AST.TBool AST.TBool exp exp'
                checkExpr (Or exp exp') = checkBinOp AST.TBool AST.TBool exp exp'

                checkBinOp :: AST.Type -> AST.Type -> AST.Expr -> AST.Expr -> TypecheckM Type
                checkBinOp rtyp etyp exp exp' = do
                    t <- checkExpr exp
                    t' <- checkExpr exp'
                    when (t /= t' || t /= etyp) $ throwError InvalidTypeError
                    return rtyp



                newScope :: TypecheckEnv -> TypecheckEnv
                newScope env = env { _currentScope = _currentScope env + 1 }

                getVar :: Ident -> TypecheckM (Type, ScopeLevel)
                getVar var = do
                    vars <- asks _vars
                    case M.lookup var vars of
                      Just v -> return v
                      Nothing -> throwError $ UndefinedVariableError var
                maybeGetVar :: Ident -> TypecheckM (Maybe (Type, ScopeLevel))
                maybeGetVar var =
                    do { v <- getVar var; return $ Just v } `catchError` (\_ -> return Nothing)
                id = do
                    env <- ask
                    return env



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
