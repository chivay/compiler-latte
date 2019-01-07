{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import qualified Abs                            as AST
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString                as B
import qualified Data.Map                       as M
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as E
import           Err
import           Numeric                        (showHex)
import           Prelude                        hiding ((<>))
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

newtype LLVMLabel =
  LLVMLabel T.Text
  deriving (Show, Eq, Ord)

newtype LLVMIdent =
  LLVMIdent T.Text
  deriving (Show, Eq)

newtype LLVMGlobalIdent =
  LLVMGlobalIdent T.Text
  deriving (Show, Eq)

instance Pretty LLVMLabel where
  pPrint (LLVMLabel label) = text $ T.unpack label

instance Pretty LLVMIdent where
  pPrint (LLVMIdent label) = char '%' <> (text $ T.unpack label)

instance Pretty LLVMGlobalIdent where
  pPrint (LLVMGlobalIdent label) = char '@' <> (text $ T.unpack label)

data LLVMType
  = I64
  | I32
  | I8
  | I1
  | Void
  | Ptr LLVMType
  | String
  | Array Integer
          LLVMType
  deriving (Show, Eq)

latteString :: LLVMType
latteString = Ptr String

data LLVMGConst =
  LLVMStringConst LLVMGlobalIdent
                  LLVMType
                  B.ByteString
  deriving (Eq, Show)

instance Pretty LLVMGConst where
  pPrint (LLVMStringConst ident typ value) =
    pPrint ident <+> char '=' <+> pPrint typ <+> parens (pPrint cVal)
    where
      cVal = concat $ (++ "\\") <$> ($ "") <$> showHex <$> (B.unpack value)

data LLVMStructDef =
  LLVMStructDef LLVMIdent
                [LLVMType]

instance Pretty LLVMStructDef where
  pPrint (LLVMStructDef name fields) =
    pPrint name <+>
    char '=' <+>
    text "type" <+> braces (hsep $ punctuate comma (pPrint <$> fields))

data LLVMExternFunc =
  LLVMExternFunc LLVMType
                 LLVMGlobalIdent
                 [LLVMType]
  deriving (Eq, Show)

instance Pretty LLVMExternFunc where
  pPrint (LLVMExternFunc rtype name args) =
    text "declare" <+>
    pPrint rtype <+>
    pPrint name <+> parens (hsep $ punctuate comma (pPrint <$> args))

latteStringDef :: LLVMStructDef
latteStringDef = LLVMStructDef (LLVMIdent "__string") [I64, Ptr I8, I32]

instance Pretty LLVMType where
  pPrint I64           = "i64"
  pPrint I32           = "i32"
  pPrint I8            = "i8"
  pPrint I1            = "i1"
  pPrint Void          = "void"
  pPrint (Ptr typ)     = pPrint typ <> char '*'
  pPrint String        = "__string"
  pPrint (Array n typ) = brackets (pPrint n <+> char 'x' <+> pPrint typ)

data LLVMValue
  = LLVMConst LLVMType
              Integer
  | LLVMReg LLVMType
            LLVMIdent
  deriving (Show, Eq)

instance Pretty LLVMValue where
  pPrint (LLVMConst _ n)   = pPrint n
  pPrint (LLVMReg _ ident) = pPrint ident

data LLVMIR
  = Ret (Maybe LLVMValue)
  | BrCond LLVMValue
           LLVMLabel
           LLVMLabel
  | Br LLVMLabel
  | Add LLVMValue
        LLVMValue
        LLVMValue
  | Sub LLVMValue
        LLVMValue
        LLVMValue
  | Mul LLVMValue
        LLVMValue
        LLVMValue
  | SDiv LLVMValue
         LLVMValue
         LLVMValue
  | SRem LLVMValue
         LLVMValue
         LLVMValue
  | Alloca LLVMValue
           LLVMType
  | Store LLVMValue
          LLVMValue
  | Load LLVMValue
         LLVMValue
  | Call LLVMValue
         LLVMGlobalIdent
         [LLVMValue]
  | ICmp CmpOp
         LLVMValue
         LLVMValue
         LLVMValue
  | And LLVMValue
        LLVMValue
        LLVMValue
  | Or LLVMValue
       LLVMValue
       LLVMValue
  | Xor LLVMValue
        LLVMValue
        LLVMValue
  deriving (Show, Eq)

data CmpOp
  = Eq
  | Ne
  | Ugt
  | Uge
  | Ult
  | Ule
  | Sgt
  | Sge
  | Sle
  | Slt
  deriving (Show, Eq)

instance Pretty CmpOp where
  pPrint Eq  = text "eq"
  pPrint Ne  = text "ne"
  pPrint Ugt = text "ugt"
  pPrint Uge = text "uge"
  pPrint Ult = text "ult"
  pPrint Ule = text "ule"
  pPrint Sgt = text "sgt"
  pPrint Sge = text "sge"
  pPrint Sle = text "sle"
  pPrint Slt = text "slt"

getType :: LLVMValue -> LLVMType
getType (LLVMConst t _) = t
getType (LLVMReg t _)   = t

labelAsIdent :: LLVMLabel -> LLVMIdent
labelAsIdent (LLVMLabel l) = LLVMIdent l

instance Pretty LLVMIR where
  pPrint (Ret Nothing) = text "ret" <+> text "void"
  pPrint (Ret (Just val)) = text "ret" <+> (pPrint . getType) val <+> pPrint val
  pPrint (Alloca res typ) =
    pPrint res <+> char '=' <+> text "alloca" <+> pPrint typ
  pPrint (Br label) =
    text "br" <+> text "label" <+> (pPrint . labelAsIdent) label
  pPrint (BrCond v label label') =
    text "br" <+>
    printType v <+>
    pPrint v <> char ',' <+>
    text "label" <+>
    (pPrint . labelAsIdent) label <> char ',' <+>
    text "label" <+> (pPrint . labelAsIdent) label'
  pPrint (Add r v v') = binOp "add" r v v'
  pPrint (Sub r v v') = binOp "sub" r v v'
  pPrint (SDiv r v v') = binOp "sdiv" r v v'
  pPrint (SRem r v v') = binOp "srem" r v v'
  pPrint (Store v vp) =
    text "store" <+> printWithType v <> char ',' <+> printWithType vp
  pPrint (Load r vp) =
    pPrint r <+>
    char '=' <+> text "load" <+> printType r <> char ',' <+> printWithType vp
  pPrint (ICmp op r c c') =
    pPrint r <+>
    char '=' <+>
    text "icmp" <+> pPrint op <+> printWithType c <> char ',' <+> pPrint c'
  pPrint (And r v v') = binOp "and" r v v'
  pPrint (Or r v v') = binOp "or" r v v'
  pPrint (Xor r v v') = binOp "xor" r v v'
  pPrint (Call r func args) =
    pPrint r <+>
    char '=' <+>
    text "call" <+>
    printType r <+>
    pPrint func <>
    parens
      (hsep $ punctuate comma ((\arg -> printType arg <+> pPrint arg) <$> args))

printType :: LLVMValue -> Doc
printType = pPrint . getType

printWithType :: LLVMValue -> Doc
printWithType v = printType v <+> pPrint v

binOp :: String -> LLVMValue -> LLVMValue -> LLVMValue -> Doc
binOp s r v v' =
  pPrint r <+>
  char '=' <+> text s <+> printType r <+> pPrint v <> char ',' <+> pPrint v'

data LLVMBlock = LLVMBlock
  { _label :: LLVMLabel
  , _insns :: [LLVMIR]
  } deriving (Show, Eq)

instance Pretty LLVMBlock where
  pPrint block =
    pPrint (_label block) <> char ':' $+$
    (indent . vcat) (pPrint <$> (_insns block))
    where
      indent = nest 4

data LLVMFuncDef =
  LLVMFuncDef LLVMType
              LLVMGlobalIdent
              [LLVMValue]
  deriving (Eq, Show)

data LLVMFunction = LLVMFunction
  { _definition :: LLVMFuncDef
  , _fblocks    :: [LLVMBlock]
  , _init       :: LLVMBlock
  } deriving (Show, Eq)

instance Pretty LLVMFunction where
  pPrint func =
    header $+$ pPrint (_init func) $+$ (vcat $ pPrint <$> (_fblocks func)) $+$
    char '}'
    where
      (LLVMFuncDef rtype name args) = _definition func
      header =
        text "define" <+>
        pPrint rtype <+>
        pPrint name <> parens (hsep $ punctuate comma (pPrint <$> args)) <+>
        char '{'

data LLVMModule = LLVMModule
  { _functions :: [LLVMFunction]
  , _globals   :: [LLVMGConst]
  , _externs   :: [LLVMExternFunc]
  } deriving (Show, Eq)

data CodegenState = CodegenState
  { _ast        :: AST.TopDef
  , _nextLabel  :: Integer
  , _nextIdent  :: Integer
  , _nextGlobal :: Integer
  , _initBlock  :: [LLVMIR]
  , _blocks     :: M.Map LLVMLabel [LLVMIR]
  , _localVars  :: [(LLVMValue)]
  }

data CodegenEnv = CodegenEnv
  { _varMap       :: M.Map AST.Ident LLVMValue
  , _currentBlock :: LLVMLabel
  }

type CodegenM = ReaderT CodegenEnv (StateT CodegenState (Except CompileError))

currentFuncName :: CodegenM AST.Ident
currentFuncName = do
  (AST.TopDef _ ident _ _) <- gets _ast
  return ident

newBlock :: CodegenM LLVMLabel
newBlock = do
  l <- newLabel
  modify (\s -> s {_blocks = M.insert l [] (_blocks s)})
  return l

newLabel :: CodegenM LLVMLabel
newLabel = do
  label <- gets _nextLabel
  modify (\s -> s {_nextLabel = label + 1})
  return $ (LLVMLabel . newLabel) label
  where
    newLabel l = ("label_" `T.append` T.pack (show l))

newGlobal :: CodegenM LLVMGlobalIdent
newGlobal = do
  ident <- gets _nextGlobal
  modify (\s -> s {_nextGlobal = ident + 1})
  funcName <- currentFuncName
  let newGlob i = (funcName `T.append` "_" `T.append` T.pack (show i))
  return $ (LLVMGlobalIdent . newGlob) ident

newIdent :: CodegenM LLVMIdent
newIdent = do
  ident <- gets _nextIdent
  modify (\s -> s {_nextIdent = ident + 1})
  return $ (LLVMIdent . newIdent) ident
  where
    newIdent l = (T.pack (show l))

getAddr :: AST.Ident -> CodegenM LLVMValue
getAddr name = do
  r <- asks ((M.lookup name) . _varMap)
  case r of
    Just val -> return val
    Nothing  -> throwError CodegenError

allocReg :: LLVMType -> CodegenM LLVMValue
allocReg typ = do
  id <- newIdent
  return (LLVMReg typ id)

allocConst :: Integer -> CodegenM LLVMValue
allocConst n = return (LLVMConst I32 n)

allocBool :: Bool -> CodegenM LLVMValue
allocBool v = return (LLVMConst I1 n)
  where
    n =
      if v
        then 1
        else 0

allocLocalVar :: LLVMType -> CodegenM LLVMValue
allocLocalVar t = do
  r <- allocReg (Ptr t)
  let alloca = Alloca r t
  modify (\s -> s {_initBlock = alloca : (_initBlock s)})
  return r

emit :: LLVMIR -> CodegenM ()
emit ins = do
  block <- asks _currentBlock
  modify
    (\s ->
       let newBlock = ins : ((_blocks s) M.! block)
        in s {_blocks = M.insert block newBlock (_blocks s)})
  return ()

load :: AST.Ident -> CodegenM LLVMValue
load name = do
  addr <- getAddr name
  res <- allocReg ((derefType . getType) addr)
  emit $ Load res addr
  return res
  where
    derefType :: LLVMType -> LLVMType
    derefType (Ptr t) = t

compileExpr :: AST.Expr -> CodegenM LLVMValue
compileExpr (AST.Var name) = load name
compileExpr (AST.LitInt n) = allocConst n
compileExpr (AST.LitString txt) = undefined
compileExpr AST.LitTrue = allocBool True
compileExpr AST.LitFalse = allocBool False
compileExpr (AST.Call ident exprs) = do
  res <- mapM compileExpr exprs
  output <- allocReg I32 -- TODO Return value
  emit $ Call output (LLVMGlobalIdent ident) res
  return output
compileExpr (AST.Neg exp) = compileExpr (AST.Add AST.Minus (AST.LitInt 0) exp)
compileExpr (AST.Not exp) = do
  e <- compileExpr exp
  r <- allocReg I1
  c <- allocConst 1
  emit $ Xor r e c
  return r
compileExpr (AST.Mul op exp exp') = do
  e <- compileExpr exp
  e' <- compileExpr exp'
  r <- allocReg (getType e)
  case op of
    AST.Times -> do
      emit $ Mul r e e'
      return r
    AST.Div -> do
      emit $ SDiv r e e'
      return r
    AST.Mod -> do
      emit $ SRem r e e'
      return r
compileExpr (AST.Add op exp exp') = do
  e <- compileExpr exp
  e' <- compileExpr exp'
  r <- allocReg (getType e)
  case op of
    AST.Plus -> do
      emit $ Add r e e'
      return r
    AST.Minus -> do
      emit $ Sub r e e'
      return r
compileExpr (AST.Comp rop exp exp') = do
  p <- compileExpr exp
  q <- compileExpr exp'
  r <- allocReg I1
  emit $ ICmp ((M.!) opMap rop) r p q
  return r
  where
    opMap =
      M.fromList
        [ (AST.Less, Slt)
        , (AST.LessEqual, Sle)
        , (AST.Greater, Sgt)
        , (AST.GreaterEqual, Sge)
        , (AST.Equal, Eq)
        , (AST.NEqual, Ne)
        ]
compileExpr (AST.And exp exp') = compileBinOp (And) exp exp'
compileExpr (AST.Or exp exp') = compileBinOp (Or) exp exp'

compileBinOp ::
     (LLVMValue -> LLVMValue -> LLVMValue -> LLVMIR)
  -> AST.Expr
  -> AST.Expr
  -> CodegenM LLVMValue
compileBinOp opcode exp exp' = do
  p <- compileExpr exp
  q <- compileExpr exp'
  r <- allocReg (getType p)
  emit $ opcode r p q
  return r

store :: LLVMValue -> LLVMValue -> CodegenM ()
store val addr = emit $ Store val addr

llvmTypeMap :: M.Map AST.Type LLVMType
llvmTypeMap =
  M.fromList
    [ (AST.TInteger, I32)
    , (AST.TBool, I1)
    , (AST.TVoid, Void)
    , (AST.TString, Ptr String)
    ]

compileStmt :: AST.Stmt -> CodegenM CodegenEnv
compileStmt AST.Empty = nop
compileStmt (AST.Ass vname expr) = do
  addr <- getAddr vname
  r <- compileExpr expr
  emit $ Store r addr
  nop
compileStmt (AST.Decl typ items) = do
  let t = (M.!) llvmTypeMap typ
  locs <- mapM allocLocalVar (replicate (length items) t)
  let items' = (\(AST.DeclItem ident mexp) -> (ident, mexp)) <$> items
  let idents = fst <$> items'
  let mexprs = (fromMaybe (AST.LitInt 0)) <$> (snd <$> items')
  results <- mapM compileExpr mexprs
  mapM_ (\(val, addr) -> store val addr) (zip results locs)
  env <- ask
  let newVars = M.fromList (zip idents locs)
  return $ env {_varMap = (M.union newVars (_varMap env))}
compileStmt (AST.Ret Nothing) = do
  emit $ Ret Nothing
  nop
compileStmt (AST.Ret (Just exp)) = do
  r <- compileExpr exp
  emit $ Ret (Just r)
  nop
compileStmt (AST.Loop exp stmt) = do
  condBlock <- newBlock
  bodyBlock <- newBlock
  postBlock <- newBlock
  emit $ Br condBlock
  local
    (setCurrentBlock condBlock)
    (do r <- compileExpr exp
        emit $ BrCond r bodyBlock postBlock)
  local
    (setCurrentBlock bodyBlock)
    (do env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br condBlock))
  env <- ask
  return (setCurrentBlock postBlock env)
compileStmt (AST.Block stmts) = do
  env <- ask
  env' <- compileStatements stmts
  let lastBlock = _currentBlock env'
  return (setCurrentBlock lastBlock env)
compileStmt (AST.Incr var) =
  compileStmt (AST.Ass var (AST.Add AST.Plus (AST.Var var) (AST.LitInt 1)))
compileStmt (AST.Decr var) =
  compileStmt (AST.Ass var (AST.Add AST.Minus (AST.Var var) (AST.LitInt 1)))
compileStmt (AST.If exp stmt) = do
  bodyBlock <- newBlock
  postBlock <- newBlock
  r <- compileExpr exp
  emit $ BrCond r bodyBlock postBlock
  local
    (setCurrentBlock bodyBlock)
    (do env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br postBlock))
  env <- ask
  return (setCurrentBlock postBlock env)
compileStmt (AST.IfElse exp stmt stmt') = do
  trueBlock <- newBlock
  falseBlock <- newBlock
  postBlock <- newBlock
  r <- compileExpr exp
  emit $ BrCond r trueBlock falseBlock
  local
    (setCurrentBlock trueBlock)
    (do env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br postBlock))
  local
    (setCurrentBlock falseBlock)
    (do env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br postBlock))
  env <- ask
  return (setCurrentBlock postBlock env)
compileStmt (AST.ExpS exp) = do
  compileExpr exp
  nop

setCurrentBlock :: LLVMLabel -> CodegenEnv -> CodegenEnv
setCurrentBlock l env = env {_currentBlock = l}

nop :: CodegenM CodegenEnv
nop = do
  env <- ask
  return env

compileStatements :: [AST.Stmt] -> CodegenM CodegenEnv
compileStatements stmts = compileAll stmts
  where
    runInEnv a e = local (const e) a
    compileAll :: [AST.Stmt] -> CodegenM CodegenEnv
    compileAll (s:ss) = do
      env <- compileStmt s
      local (const env) (compileAll ss)
    compileAll [] = nop

mangleFunctionName :: AST.Ident -> LLVMGlobalIdent
mangleFunctionName ident = LLVMGlobalIdent (T.append "latte_" ident)

generateFirstBlock :: CodegenM (CodegenEnv, LLVMFuncDef)
generateFirstBlock = do
  (AST.TopDef rtype fname args _) <- gets _ast
  env <- ask
  let rtyp' = (M.!) llvmTypeMap rtype
  let fname' = mangleFunctionName fname
  args' <- mapM argAlloc args
  return (env, LLVMFuncDef rtyp' fname' args')
  where
    argAlloc :: AST.TypVar -> CodegenM LLVMValue
    argAlloc (AST.TypVar typ ident) = do
      let t = (M.!) llvmTypeMap typ
      allocReg t

generateCode :: CodegenM LLVMFunction
generateCode = do
  (startenv, def) <- generateFirstBlock
  (AST.TopDef _ _ _ stmts) <- gets _ast
  entry <- newBlock
  local (setCurrentBlock entry) (compileStatements stmts)
  initInsns <- gets _initBlock
  let initInsns' = (Br entry) : initInsns
  blocks <- (gets _blocks)
  let blocks' = (\(l, insns) -> LLVMBlock l (reverse insns)) <$> M.toList blocks
  return
    (LLVMFunction
       { _definition = def
       , _init = LLVMBlock (LLVMLabel "init") (reverse initInsns')
       , _fblocks = blocks'
       })
