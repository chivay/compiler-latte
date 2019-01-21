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
  deriving (Show, Eq, Ord)

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
  | Struct [LLVMType]
  deriving (Show, Eq)

latteString :: LLVMType
latteString = Ptr String

makeLatteArray :: LLVMType -> LLVMType
makeLatteArray t = Ptr $ Struct [I32, t]

data LLVMGConst =
  LLVMStringConst LLVMValue
                  B.ByteString
  deriving (Eq, Show)

instance Pretty LLVMGConst where
  pPrint (LLVMStringConst llval dat) =
    pPrint llval <+>
    char '=' <+>
    text "constant" <+> printType llval <+> char 'c' <> doubleQuotes (text cVal)
    where
      cVal =
        concat $ (\s -> '\\' : s) <$> ($ "") <$> (showHex <$> (B.unpack dat))

data LLVMStructDef =
  LLVMStructDef LLVMIdent
                [LLVMType]
  deriving (Eq, Show)

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

instance Pretty LLVMType where
  pPrint I64           = "i64"
  pPrint I32           = "i32"
  pPrint I8            = "i8"
  pPrint I1            = "i1"
  pPrint Void          = "void"
  pPrint (Ptr typ)     = pPrint typ <> char '*'
  pPrint String        = "%__string"
  pPrint (Array n typ) = brackets (pPrint n <+> char 'x' <+> pPrint typ)
  pPrint (Struct ts)   = (braces . hcat) (punctuate comma (pPrint <$> ts))

data LLVMValue
  = LLVMConst LLVMType
              Integer
  | LLVMReg LLVMType
            LLVMIdent
  | LLVMGlobal LLVMType
               LLVMGlobalIdent
  deriving (Show, Eq)

instance Pretty LLVMValue where
  pPrint (LLVMConst _ n)      = pPrint n
  pPrint (LLVMReg _ ident)    = pPrint ident
  pPrint (LLVMGlobal _ ident) = pPrint ident

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
  | Bitcast LLVMValue
            LLVMValue
  | Gep LLVMValue
        LLVMValue
        [LLVMValue]
  | Comment T.Text
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
getType (LLVMConst t _)  = t
getType (LLVMReg t _)    = t
getType (LLVMGlobal t _) = t

derefType :: LLVMType -> LLVMType
derefType (Ptr t) = t

instance Pretty LLVMIR where
  pPrint (Ret Nothing) = text "ret" <+> text "void"
  pPrint (Ret (Just val)) = text "ret" <+> (pPrint . getType) val <+> pPrint val
  pPrint (Alloca res typ) =
    pPrint res <+> char '=' <+> text "alloca" <+> pPrint typ
  pPrint (Br label) = text "br" <+> text "label" <+> char '%' <> pPrint label
  pPrint (BrCond v label label') =
    text "br" <+>
    printType v <+>
    pPrint v <> char ',' <+>
    text "label" <+>
    char '%' <> pPrint label <> char ',' <+>
    text "label" <+> char '%' <> pPrint label'
  pPrint (Add r v v') = binOp "add" r v v'
  pPrint (Sub r v v') = binOp "sub" r v v'
  pPrint (Mul r v v') = binOp "mul" r v v'
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
    prefix <+>
    pPrint func <>
    parens
      (hsep $ punctuate comma ((\arg -> printType arg <+> pPrint arg) <$> args))
    where
      prefix =
        case getType r of
          Void -> text "call" <+> text "void"
          typ  -> pPrint r <+> char '=' <+> text "call" <+> printType r
  pPrint (Bitcast r s) =
    pPrint r <+>
    char '=' <+>
    text "bitcast" <+> printWithType s <+> text "to" <+> printType r
  pPrint (Gep r s idxs) =
    pPrint r <+>
    char '=' <+>
    text "getelementptr" <+>
    (pPrint . derefType . getType) s <> char ',' <+>
    printWithType s <> char ',' <+>
    (hsep $ punctuate comma (printWithType <$> idxs))
  pPrint (Comment comment) = text "//" <+> text (T.unpack comment)

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
        pPrint name <>
        parens
          (hsep $
           punctuate comma ((\arg -> printType arg <+> pPrint arg) <$> args)) <+>
        char '{'

data LLVMModule = LLVMModule
  { _functions :: [LLVMFunction]
  , _globals   :: [LLVMGConst]
  , _externs   :: [LLVMExternFunc]
  , _structs   :: [LLVMStructDef]
  } deriving (Show, Eq)

instance Pretty LLVMModule where
  pPrint mod =
    vcat (pPrint <$> (_globals mod)) $+$ vcat (pPrint <$> (_structs mod)) $+$
    vcat (pPrint <$> (_externs mod)) $+$
    vcat (pPrint <$> (_functions mod))

data CodegenState = CodegenState
  { _ast          :: AST.TopDef
  , _nextLabel    :: Integer
  , _nextIdent    :: Integer
  , _nextGlobal   :: Integer
  , _initBlock    :: [LLVMIR]
  , _blocks       :: M.Map LLVMLabel [LLVMIR]
  , _globalDefs   :: [LLVMGConst]
  , _currentBlock :: LLVMLabel
  }

data CodegenEnv = CodegenEnv
  { _varMap  :: M.Map AST.Ident (LLVMValue, AST.Type)
  , _funcRet :: M.Map LLVMGlobalIdent (LLVMGlobalIdent, LLVMType)
  } deriving (Show)

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

newGlobal :: LLVMType -> CodegenM LLVMValue
newGlobal t = do
  ident <- gets _nextGlobal
  modify (\s -> s {_nextGlobal = ident + 1})
  funcName <- currentFuncName
  let newGlob =
        ((mangleFunctionName funcName) `T.append` "_" `T.append`
         T.pack (show ident))
  return $ LLVMGlobal (Ptr t) (LLVMGlobalIdent newGlob)

newIdent :: CodegenM LLVMIdent
newIdent = do
  ident <- gets _nextIdent
  modify (\s -> s {_nextIdent = ident + 1})
  return $ (LLVMIdent . newIdent) ident
  where
    newIdent l = (T.pack (show l))

getAddr :: AST.LValue -> CodegenM LLVMValue
getAddr (AST.Var name) = do
  r <- asks ((M.lookup name) . _varMap)
  case r of
    Just (val, _) -> return val
    Nothing       -> throwError $ CodegenError (show name)
getAddr (AST.Indexed expr lvalue) = do
  offset <- compileExpr expr
  arrPtr <- load lvalue
  r <- allocReg (Ptr I8)
  let subT = ((getElemType.getType) arrPtr)
  res <- allocReg subT
  emit $ Bitcast r arrPtr
  rPtr <- allocReg (Ptr I8)
  emit $ Call rPtr (LLVMGlobalIdent "__get_array_buffer") [r]
  cPtr <- allocReg (Ptr ((getElemType.getType) arrPtr))
  emit $ Bitcast cPtr rPtr
  resAddr <- allocReg (Ptr subT)
  emit $ Gep resAddr cPtr [offset]
  return resAddr
  where
    getElemType (Ptr (Struct (I32:t:[]))) = t

allocReg :: LLVMType -> CodegenM LLVMValue
allocReg typ = do
  (LLVMIdent id) <- newIdent
  let rid = LLVMIdent ("r" `T.append` id)
  return (LLVMReg typ rid)

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
  block <- gets _currentBlock
  case block of
    (LLVMLabel "init") -> modify (\s -> s {_initBlock = ins : (_initBlock s)})
    _ ->
      modify
        (\s ->
           let newBlock = ins : ((_blocks s) M.! block)
            in s {_blocks = M.insert block newBlock (_blocks s)})

load :: AST.LValue -> CodegenM LLVMValue
load (AST.Field "length" arr) = do
  ref <- load arr
  res <- allocReg I32
  ptr <- allocReg (Ptr I8)
  emit $ Bitcast ptr ref
  emit $ Call res (LLVMGlobalIdent "__get_array_length") [ptr]
  return res
load (AST.Indexed expr lvalue) = do
  offset <- compileExpr expr
  arrPtr <- load lvalue
  r <- allocReg (Ptr I8)
  let subT = ((getElemType . getType) arrPtr)
  res <- allocReg subT
  emit $ Bitcast r arrPtr
  rPtr <- allocReg (Ptr I8)
  emit $ Call rPtr (LLVMGlobalIdent "__get_array_buffer") [r]
  cPtr <- allocReg (Ptr ((getElemType . getType) arrPtr))
  emit $ Bitcast cPtr rPtr
  resAddr <- allocReg (Ptr subT)
  emit $ Gep resAddr cPtr [offset]
  emit $ Load res resAddr
  return res
  where
    getElemType (Ptr (Struct (I32:t:[]))) = t
load lval = do
  addr <- getAddr lval
  res <- allocReg ((derefType . getType) addr)
  emit $ Load res addr
  return res

allocStringConst :: T.Text -> CodegenM LLVMValue
allocStringConst txt = do
  g <- newGlobal (Array bufLen I8)
  let (LLVMGlobal _ id) = g
  modify
    (\s ->
       s
         { _globalDefs =
             (LLVMStringConst (LLVMGlobal (Array bufLen I8) id) buf) :
             (_globalDefs s)
         })
  r <- allocReg (Ptr I8)
  emit $ Bitcast r g
  return r
  where
    bufLen = toInteger $ B.length buf
    buf = E.encodeUtf8 txt

compileExpr :: AST.Expr -> CodegenM LLVMValue
compileExpr expr@(AST.Mem lvalue) = do
  val <- load lvalue
  return val
compileExpr (AST.LitInt n) = allocConst n
compileExpr (AST.LitString txt) = do
  stringPtr <- allocReg latteString
  dummy <- allocReg Void
  buf <- allocStringConst txt
  emit $ Call stringPtr (LLVMGlobalIdent "__alloc_string") []
  emit $
    Call
      dummy
      (LLVMGlobalIdent "__init_string")
      [stringPtr, buf, LLVMConst I64 bufLen, LLVMConst I1 0]
  return stringPtr
  where
    bufLen = toInteger $ B.length $ E.encodeUtf8 txt
compileExpr AST.LitTrue = allocBool True
compileExpr AST.LitFalse = allocBool False
compileExpr (AST.Call ident exprs) = do
  res <- mapM compileExpr exprs
  funcs <- asks _funcRet
  let (mident, rtype) = (M.!) funcs (LLVMGlobalIdent ident)
  output <- allocReg rtype
  emit $ Call output mident res
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
    AST.Plus ->
      if (getType e) == latteString
        then concatStrings r e e'
        else do
          emit $ Add r e e'
          return r
    AST.Minus -> do
      emit $ Sub r e e'
      return r
  where
    concatStrings r e e' = do
      emit $ Call r (LLVMGlobalIdent "__concat_strings") [e, e']
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
compileExpr (AST.And exp exp') = do
  loc <- allocLocalVar I1
  doSecond <- newBlock
  postBlock <- newBlock
  store (LLVMConst I1 0) loc
  p <- compileExpr exp
  emit $ BrCond p doSecond postBlock
  setCurrentBlock doSecond
  r <- compileExpr exp'
  store r loc
  emit $ Br postBlock
  setCurrentBlock postBlock
  r <- allocReg I1
  emit $ Load r loc
  return r
compileExpr (AST.Or exp exp') = do
  loc <- allocLocalVar I1
  doSecond <- newBlock
  postBlock <- newBlock
  store (LLVMConst I1 1) loc
  p <- compileExpr exp
  emit $ BrCond p postBlock doSecond
  setCurrentBlock doSecond
  r <- compileExpr exp'
  store r loc
  emit $ Br postBlock
  setCurrentBlock postBlock
  r <- allocReg I1
  emit $ Load r loc
  return r
compileExpr (AST.New (AST.TArray (Just size) arrType)) = do
  arr <- allocReg (Ptr I8)
  res <- allocReg (makeLatteArray baseType)
  arrSize <- allocConst size
  elemSize <- allocConst baseTypeSize
  emit $ Call arr (LLVMGlobalIdent "__alloc_array") [arrSize, elemSize]
  emit $ Bitcast res arr
  return res
  where
    baseType = getLLVMType arrType
    baseTypeSize =
      case arrType of
        AST.TInteger -> 4
        AST.TBool    -> 1
        AST.TString  -> 8

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

getLLVMType :: AST.Type -> LLVMType
getLLVMType AST.TInteger     = I32
getLLVMType AST.TBool        = I1
getLLVMType AST.TVoid        = Void
getLLVMType AST.TString      = Ptr String
getLLVMType (AST.TArray _ t) = makeLatteArray (getLLVMType t)

compileStmt :: AST.Stmt -> CodegenM CodegenEnv
compileStmt AST.Empty = nop
compileStmt (AST.Ass lvalue expr) = do
  addr <- getAddr lvalue
  r <- compileExpr expr
  emit $ Store r addr
  nop
-- REFACTOR THIS
compileStmt (AST.Decl typ items) = do
  let t = getLLVMType typ
  locs <- mapM allocLocalVar (replicate (length items) t)
  let items' = (\(AST.DeclItem ident mexp) -> (ident, mexp)) <$> items
  let idents = fst <$> items'
  let mexprs =
        case typ of
          AST.TInteger -> (fromMaybe (AST.LitInt 0)) <$> (snd <$> items')
          AST.TBool    -> (fromMaybe (AST.LitFalse)) <$> (snd <$> items')
          AST.TString  -> (fromMaybe (AST.LitString "")) <$> (snd <$> items')
          _            -> catMaybes $ (snd <$> items')
  results <- mapM compileExpr mexprs
  mapM_ (\(val, addr) -> store val addr) (zip results locs)
  env <- ask
  let newVars = M.fromList (zip idents (zip locs (replicate (length locs) typ)))
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
  setCurrentBlock condBlock
  r <- compileExpr exp
  emit $ BrCond r bodyBlock postBlock
  setCurrentBlock bodyBlock
  compileStmt stmt
  emit $ Br condBlock
  setCurrentBlock postBlock
  nop
compileStmt (AST.Block stmts) = do
  compileStatements stmts
  nop
compileStmt (AST.Incr var) =
  compileStmt (AST.Ass var (AST.Add AST.Plus (AST.Mem var) (AST.LitInt 1)))
compileStmt (AST.Decr var) =
  compileStmt (AST.Ass var (AST.Add AST.Minus (AST.Mem var) (AST.LitInt 1)))
compileStmt (AST.If AST.LitTrue stmt) = compileStmt stmt
compileStmt (AST.If AST.LitFalse _) = nop
compileStmt stmT@(AST.If exp stmt) = do
  bodyBlock <- newBlock
  postBlock <- newBlock
  r <- compileExpr exp
  emit $ BrCond r bodyBlock postBlock
  setCurrentBlock bodyBlock
  compileStmt stmt
  emit $ Br postBlock
  setCurrentBlock postBlock
  nop
compileStmt (AST.IfElse AST.LitTrue stmt _) = compileStmt stmt
compileStmt (AST.IfElse AST.LitFalse _ stmt) = compileStmt stmt
compileStmt stmtT@(AST.IfElse exp stmt stmt') = do
  trueBlock <- newBlock
  falseBlock <- newBlock
  postBlock <- newBlock
  r <- compileExpr exp
  emit $ BrCond r trueBlock falseBlock
  setCurrentBlock trueBlock
  compileStmt stmt
  when (not $ willReturn stmtT) (emit $ Br postBlock)
  setCurrentBlock falseBlock
  compileStmt stmt'
  when (not $ willReturn stmtT) (emit $ Br postBlock)
  setCurrentBlock postBlock
  nop
compileStmt (AST.ExpS exp) = do
  compileExpr exp
  nop

setCurrentBlock :: LLVMLabel -> CodegenM ()
setCurrentBlock l = do
  modify (\s -> s {_currentBlock = l})

nop :: CodegenM CodegenEnv
nop = do
  env <- ask
  return env

willReturn :: AST.Stmt -> Bool
willReturn (AST.Ret _) = True
willReturn (AST.ExpS (AST.Call "error" _)) = True
willReturn (AST.Block stmts) = any willReturn stmts
willReturn (AST.If AST.LitTrue stmt) = willReturn stmt
willReturn (AST.IfElse AST.LitTrue stmt _) = willReturn stmt
willReturn (AST.IfElse AST.LitFalse _ stmt) = willReturn stmt
willReturn (AST.IfElse _ stmt stmt') = willReturn stmt && willReturn stmt'
willReturn _ = False

compileStatements :: [AST.Stmt] -> CodegenM CodegenEnv
compileStatements stmts = compileAll stmts
  where
    runInEnv a e = local (const e) a
    compileAll :: [AST.Stmt] -> CodegenM CodegenEnv
    compileAll (s:ss) = do
      env <- compileStmt s
      if willReturn s
        then nop
        else local (const env) (compileAll ss)
    compileAll [] = nop

mangleFunctionName :: AST.Ident -> AST.Ident
mangleFunctionName ident = T.append "latte_" ident

generateFirstBlock :: CodegenM (CodegenEnv, LLVMFuncDef)
generateFirstBlock = do
  (AST.TopDef rtype fname args _) <- gets _ast
  env <- ask
  let rtyp' = getLLVMType rtype
  let fname' = mangleFunctionName fname
  args' <- mapM argRegAlloc args
  locs <- mapM allocLocalVar (getType <$> args')
  mapM (\(r, addr) -> store r addr) (zip args' locs)
  let (types, idents) = unpackArgs args
  let vars = M.fromList (zip idents (zip locs types))
  return
    (env {_varMap = vars}, LLVMFuncDef rtyp' (LLVMGlobalIdent fname') args')
  where
    argRegAlloc :: AST.TypVar -> CodegenM LLVMValue
    argRegAlloc (AST.TypVar typ ident) = do
      let t = getLLVMType typ
      allocReg t
    unpackArgs :: [AST.TypVar] -> ([AST.Type], [AST.Ident])
    unpackArgs args = unzip $ fmap unpack args
      where
        unpack (AST.TypVar t i) = (t, i)

generateCode :: CodegenM LLVMFunction
generateCode = do
  (startenv, def) <- generateFirstBlock
  (AST.TopDef rtype _ _ stmts) <- gets _ast
  entry <- newBlock
  let stmts' =
        if rtype == AST.TVoid
          then stmts ++ [AST.Ret Nothing]
          else stmts
  setCurrentBlock entry
  local (const $ startenv) (compileStatements stmts')
  initInsns <- gets _initBlock
  let initInsns' = (Br entry) : initInsns
  blocks <- (gets _blocks)
  let blocks' =
        (\(l, insns) -> LLVMBlock l (reverse insns)) <$>
        pruneEmptyBlocks (M.toList blocks)
  return
    (LLVMFunction
       { _definition = def
       , _init = LLVMBlock (LLVMLabel "init") (reverse initInsns')
       , _fblocks = blocks'
       })
  where
    pruneEmptyBlocks :: [(LLVMLabel, [LLVMIR])] -> [(LLVMLabel, [LLVMIR])]
    pruneEmptyBlocks = filter (\(_, insns) -> not $ null insns)
