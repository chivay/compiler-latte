{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler where

import           Abs                            as AST
import qualified Codegen                        as C
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                       as M
import           Data.Monoid
import qualified Data.Text                      as T
import           Debug.Trace
import           Err
import           Prelude                        hiding ((<>))
import           Prettify
import           System.Exit
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

type CompilerM = ExceptT CompileError (StateT CompilerState IO)

data CompilerState = CompilerState
  { _filename :: FilePath
  , _src      :: T.Text
  , _ast      :: AST.Program
  , _topDefs  :: M.Map Ident Type
  , _strDefs  :: M.Map Ident (M.Map Ident Type)
  , _code     :: Maybe C.LLVMModule
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
    addDef (FuncDef rtype ident args _) = do
      tds <- gets _topDefs
      if M.member ident tds
        then throwError (FunctionRedefinitionError ident)
        else modify insertFunc
      where
        ftype = AST.TFunc rtype (removeIdents args)
        insertFunc s = s {_topDefs = M.insert ident ftype (_topDefs s)}
    addDef (StructDef ident body) = do
      strs <- gets _strDefs
      if M.member ident strs
        then throwError (StructRedefinitionError ident)
        else modify insertStruct
      where
        stype = M.fromList fields
        fields = (\(TypVar t i) -> (i, t)) <$> body
        insertStruct s = s {_strDefs = M.insert ident stype (_strDefs s)}

assignCompatible :: Type -> Type -> Bool
assignCompatible (TArray _ subT) (TArray _ subT') = assignCompatible subT subT'
assignCompatible t t' = t == t'

checkTopDefinitions :: CompilerM ()
checkTopDefinitions = do
  (AST.Program tds) <- gets _ast
  forM_ tds checkDef
  where
    isLegalType :: Type -> CompilerM Bool
    isLegalType (AST.TStruct ident) = do
        strs <- gets _strDefs
        return $ ident `M.member` strs
    isLegalType _ = return True

    checkDef :: AST.TopDef -> CompilerM ()
    checkDef (AST.StructDef sname fields) = mapM_ checkField fields
      where checkField (TypVar t ident) = do
              typeOk <- isLegalType t
              when (not typeOk) $ throwError $ UndefinedType t
              return()               -- TODO
    checkDef (AST.FuncDef rtype fname args body) = do
      tds <- gets _topDefs
      let initVars =
            M.fromList (fmap (\(TypVar typ name) -> (name, (typ, 0))) args)
      when (M.size initVars /= length args) $ throwError RedefinitionError
      let initialEnv =
            TypecheckEnv
              { _funcDefs = tds
              , _vars = initVars
              , _currentScope = 0
              , _returnType = rtype
              }
      case runExcept (runReaderT (checkAll initialEnv body) initialEnv) of
        (Left e) -> throwError e
        _        -> return ()
      where
        runInEnv a e = local (const e) a
        checkAll :: TypecheckEnv -> [AST.Stmt] -> TypecheckM TypecheckEnv
        checkAll env stmts =
          (foldl
             (>=>)
             (const $ return env)
             ((runInEnv . checkWithErrorLoc) <$> stmts))
            env
        checkWithErrorLoc :: AST.Stmt -> TypecheckM TypecheckEnv
        checkWithErrorLoc stmt = do
          (checkStmt stmt) `catchError`
            (\e -> throwError $ StmtLocatedError stmt e)
        checkStmt :: AST.Stmt -> TypecheckM TypecheckEnv
        checkStmt (Block stmts) = do
          env <- ask
          checkAll (newScope env) stmts
          id
        checkStmt (Decl typ items) = do
          env <- ask
          forM_ items checkItemType
          when (typ == AST.TVoid) $
            throwError $ InvalidTypeError $ "void variable"
          vars' <- foldM newScopedVar (_vars env) items
          return $ env {_vars = vars'}
          where
            newScopedVar :: VEnv -> DeclItem -> TypecheckM VEnv
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
              when (not $ typ `assignCompatible` et) $
                throwError $ TypeError $ (T.pack . show) typ
            checkItemType (DeclItem _ Nothing) = return () -- Default initialization
        checkStmt (Ass lvalue expr) = do
          typ <- resolveLValue lvalue
          typ' <- checkExpr expr
          when (not $ typ `assignCompatible` typ') $
            throwError $ InvalidTypeError "assignment to wrong type"
          id
        checkStmt (Incr var) = do
          typ <- resolveLValue var
          when
            (typ /= AST.TInteger)
            (throwError $ InvalidTypeError "not an integer")
          id
        checkStmt (Decr var) = do
          typ <- resolveLValue var
          when
            (typ /= AST.TInteger)
            (throwError $ InvalidTypeError "not an integer")
          id
        checkStmt (Ret Nothing) = do
          rtyp <- asks _returnType
          when
            (rtyp /= AST.TVoid)
            (throwError $ InvalidTypeError "cannot return void")
          id
        checkStmt Empty = id
        checkStmt (Ret (Just expr)) = do
          rtype <- asks _returnType
          etype <- checkExpr expr
          when
            (rtype /= etype)
            (throwError $ InvalidTypeError "unexpected type")
          id
        checkStmt (If expr stmt) = do
          etype <- checkExpr expr
          when (etype /= AST.TBool) (throwError $ InvalidTypeError "not a bool")
          checkStmt stmt
          id
        checkStmt (IfElse expr stmt stmt') = do
          etype <- checkExpr expr
          when (etype /= AST.TBool) (throwError $ InvalidTypeError "not a bool")
          checkStmt stmt
          checkStmt stmt'
          id
        checkStmt (Loop expr stmt) = do
          etype <- checkExpr expr
          when (etype /= AST.TBool) (throwError $ InvalidTypeError "not a bool")
          checkStmt stmt
          id
        checkStmt (Foreach (TypVar typ ident) arr stmt) = do
          arrT <- checkExpr arr
          when
            ((not . isArray) arrT)
            (throwError $ InvalidTypeError "only arrays are iterable")
          let (TArray _ elT) = arrT
          when (elT /= typ) (throwError $ InvalidTypeError "conflicting types")
          id
          where
            isArray (TArray _ _) = True
            isArray _            = False
        checkStmt (ExpS expr) = do
          checkExpr expr
          id
        checkExpr :: Expr -> TypecheckM Type
        checkExpr (Mem lvalue) = do
          typ <- resolveLValue lvalue
          return typ
        checkExpr (LitInt _) = return AST.TInteger
        checkExpr LitTrue = return AST.TBool
        checkExpr LitFalse = return AST.TBool
        checkExpr (Call fname exprs) = do
          fenv <- asks _funcDefs
          params <- mapM checkExpr exprs
          case M.lookup fname fenv of
            Just (TFunc rt ats) -> do
              when (params /= ats) $
                throwError $ InvalidTypeError "invalid call"
              return rt
            Nothing -> throwError $ UndefinedVariableError fname
        checkExpr (New t@(TArray (Just e) _)) = do
          et <- checkExpr e
          when (et /= TInteger) $
            throwError $ TypeError "Array size must be an integer"
          return t
        checkExpr (LitString _) = return AST.TString
        checkExpr (Neg expr) = do
          t <- checkExpr expr
          when (t /= AST.TInteger) $
            throwError $ InvalidTypeError "not an integer"
          return AST.TInteger
        checkExpr (Not expr) = do
          t <- checkExpr expr
          when (t /= AST.TBool) $ throwError $ InvalidTypeError "not a bool"
          return AST.TBool
        checkExpr (Mul _ exp exp') =
          checkBinOp AST.TInteger [AST.TInteger] exp exp'
        checkExpr (Add AST.Plus exp exp') = do
          t <- checkExpr exp
          t' <- checkExpr exp'
          when (t /= t') $ throwError $ InvalidTypeError "types not matching"
          unless (elem t [AST.TInteger, AST.TString]) $
            throwError $ InvalidTypeError "no plus operator"
          return t
        checkExpr (Add _ exp exp') =
          checkBinOp AST.TInteger [AST.TInteger] exp exp'
        checkExpr (Comp _ exp exp') =
          checkBinOp AST.TBool [AST.TInteger, AST.TBool] exp exp'
        checkExpr (And exp exp') = checkBinOp AST.TBool [AST.TBool] exp exp'
        checkExpr (Or exp exp') = checkBinOp AST.TBool [AST.TBool] exp exp'
        checkBinOp ::
             AST.Type -> [AST.Type] -> AST.Expr -> AST.Expr -> TypecheckM Type
        checkBinOp rtyp etyps exp exp' = do
          t <- checkExpr exp
          t' <- checkExpr exp'
          when (t /= t') $ throwError $ InvalidTypeError "conflicting types"
          unless (elem t etyps) $ throwError $ InvalidTypeError "not supported"
          return rtyp
        newScope :: TypecheckEnv -> TypecheckEnv
        newScope env = env {_currentScope = _currentScope env + 1}
        resolveLValue :: LValue -> TypecheckM Type
        resolveLValue (Var var) = do
          vars <- asks _vars
          case M.lookup var vars of
            Just v  -> return $ fst v
            Nothing -> throwError $ UndefinedVariableError var
        resolveLValue (Indexed _ lvalue) = do
          baseType <- resolveLValue lvalue
          case baseType of
            (TArray _ typ) -> return typ
            _              -> throwError $ TypeError "Not an array!"
        resolveLValue (Field field lvalue) = do
          baseType <- resolveLValue lvalue
          case baseType of
            (TArray _ _)
              | (field == "length") -> return TInteger
            _ -> throwError $ TypeError "Structs unimplemented"
        id = do
          env <- ask
          return env

checkMain :: CompilerM ()
checkMain = do
  tds <- gets _topDefs
  case M.lookup "main" tds of
    Just (TFunc AST.TInteger []) -> return ()
    Just _ -> throwError $ TypeError "wrong main type"
    Nothing -> throwError $ UndefinedFunctionError "main"

checkReturnPaths :: CompilerM ()
checkReturnPaths = do
  (AST.Program tds) <- gets _ast
  forM_ tds checkReturn
  where
    checkReturn :: AST.TopDef -> CompilerM ()
    checkReturn (FuncDef rtype _ _ stmts) = do
      let (nret, ret) = break willReturn stmts
      when
        (rtype /= AST.TVoid)
        (case ret of
           []  -> throwError ReturnPathError
           [_] -> return ()
           _   -> return ())
    willReturn :: Stmt -> Bool
    willReturn (Ret _)                  = True
    willReturn (ExpS (Call "error" _))  = True
    willReturn (Block stmts)            = any willReturn stmts
    willReturn (If LitTrue stmt)        = willReturn stmt
    willReturn (IfElse LitTrue stmt _)  = willReturn stmt
    willReturn (IfElse LitFalse _ stmt) = willReturn stmt
    willReturn (IfElse _ stmt stmt')    = willReturn stmt && willReturn stmt'
    willReturn _                        = False

compileFunction :: AST.TopDef -> CompilerM (C.LLVMFunction, [C.LLVMGConst])
compileFunction td = do
  localFunctions <- gets _topDefs
  let initialEnv =
        C.CodegenEnv
          { C._varMap = M.empty
          , C._funcRet =
              (M.fromList
                 [ libraryFunction "printInt" C.Void
                 , libraryFunction "printString" C.Void
                 , libraryFunction "error" C.Void
                 , libraryFunction "readInt" C.I32
                 , libraryFunction "readString" C.latteString
                 ]) `M.union`
              (toCodegenDefs localFunctions)
          }
  case runExcept (runStateT (runReaderT C.generateCode initialEnv) initialState) of
    (Left e)          -> throwError e
    (Right (func, s)) -> return (func, C._globalDefs s)
  where
    libraryFunction ::
         T.Text
      -> C.LLVMType
      -> (C.LLVMGlobalIdent, (C.LLVMGlobalIdent, C.LLVMType))
    libraryFunction func ret = (glob func, (mangledIdent, ret))
      where
        mangledIdent = glob ("__latte_std_" `T.append` func)
        glob = C.LLVMGlobalIdent
    localFunction ::
         T.Text
      -> C.LLVMType
      -> (C.LLVMGlobalIdent, (C.LLVMGlobalIdent, C.LLVMType))
    localFunction func ret = (glob func, (mangledIdent, ret))
      where
        mangledIdent = glob ("latte_" `T.append` func)
        glob = C.LLVMGlobalIdent
    toCodegenDefs ::
         M.Map Ident Type
      -> M.Map C.LLVMGlobalIdent (C.LLVMGlobalIdent, C.LLVMType)
    toCodegenDefs m = M.fromList mangledIdents
      where
        funcs = M.toList m
        (idents, types) = unzip funcs
        llvmIdents = (\i -> C.LLVMGlobalIdent i) <$> idents
        mangledIdents = (\(f, t) -> f t) <$> (zip partial types')
        partial = localFunction <$> idents
        llvmTypes = C.getLLVMType
        types' = llvmTypes <$> (\(TFunc r _) -> r) <$> types
        values = zip mangledIdents types'
    initialState =
      C.CodegenState
        { C._ast = td
        , C._nextLabel = 0
        , C._nextIdent = 0
        , C._nextGlobal = 0
        , C._initBlock = []
        , C._globalDefs = []
        , C._currentBlock = C.LLVMLabel "init"
        , C._blocks = M.empty
        }

generateCode :: CompilerM ()
generateCode = do
  (AST.Program tds) <- gets _ast
  output <- mapM compileFunction tds
  let funcs = fst <$> output
  let globals = foldl (++) [] (snd <$> output)
  let mod =
        C.LLVMModule
          { C._functions = funcs
          , C._globals = globals
          , C._externs =
              [ C.LLVMExternFunc C.Void (func "__latte_std_printInt") [C.I32]
              , C.LLVMExternFunc
                  C.Void
                  (func "__latte_std_printString")
                  [C.latteString]
              , C.LLVMExternFunc C.I32 (func "__latte_std_readInt") []
              , C.LLVMExternFunc
                  C.latteString
                  (func "__latte_std_readString")
                  []
              , C.LLVMExternFunc C.latteString (func "__alloc_string") []
              , C.LLVMExternFunc
                  C.latteString
                  (func "__concat_strings")
                  [C.latteString, C.latteString]
              , C.LLVMExternFunc
                  C.Void
                  (func "__init_string")
                  [C.latteString, C.Ptr (C.I8), C.I64, C.I1]
              , C.LLVMExternFunc C.Void (func "__latte_std_error") []
              , C.LLVMExternFunc
                  (C.Ptr C.I8)
                  (func "__alloc_array")
                  [C.I32, C.I32]
              , C.LLVMExternFunc
                  (C.I32)
                  (func "__get_array_length")
                  [C.Ptr C.I8]
              , C.LLVMExternFunc
                  (C.Ptr C.I8)
                  (func "__get_array_buffer")
                  [C.Ptr C.I8]
              ]
          , C._structs =
              [ C.LLVMStructDef
                  (C.LLVMIdent "__string")
                  [C.I64, C.Ptr (C.I8), C.I32]
              ]
          }
  liftIO $ (print . pPrint) mod
  return ()
  where
    func = C.LLVMGlobalIdent

compileProgram :: AST.Program -> IO ()
compileProgram prog = do
  (e, s) <- runStateT (runExceptT compile) initialState
  case e of
    Left e -> do
      (print . pPrint) e
      exitWith $ ExitFailure 1
    _ -> return ()
  where
    compile = do
      printString "Loading functions definitions..."
      loadTopDefinitions
      printString "Typechecking functions..."
      checkTopDefinitions
      printString "Checking return paths..."
      checkReturnPaths
      printString "Generating code..."
      generateCode
    printString :: String -> CompilerM ()
    printString _ = return ()
    initialState =
      CompilerState
        { _filename = "prog.lat"
        , _src = "somethin"
        , _ast = prog
        , _topDefs = initialDefinitions
        , _strDefs = M.empty
        , _code = Nothing
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
