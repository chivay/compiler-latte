{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import qualified Abs                  as AST
import qualified Data.Text            as T
import qualified Data.Map             as M
import           Prelude              hiding ((<>))
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass
import           Data.Maybe
import           Err

newtype LLVMLabel = LLVMLabel T.Text deriving (Show, Eq, Ord)
newtype LLVMIdent = LLVMIdent T.Text deriving (Show, Eq)
newtype LLVMFuncIdent = LLVMFuncIdent T.Text deriving (Show, Eq)

instance Pretty LLVMLabel where
    pPrint (LLVMLabel label) = text $ T.unpack label

instance Pretty LLVMIdent where
    pPrint (LLVMIdent label) = char '%' <> (text $ T.unpack label)

instance Pretty LLVMFuncIdent where
    pPrint (LLVMFuncIdent label) = char '@' <> (text $ T.unpack label)

data LLVMType = I32
              | I1
              | Void
              | Ptr LLVMType
              deriving (Show, Eq)

instance Pretty LLVMType where
    pPrint I32 = "i32"
    pPrint I1 = "i1"
    pPrint (Ptr typ) = pPrint typ <> char '*'

data LLVMValue = LLVMConst LLVMType Integer
               | LLVMReg   LLVMType LLVMIdent
               deriving (Show, Eq)


instance Pretty LLVMValue where
    pPrint (LLVMConst _ n) = pPrint n
    pPrint (LLVMReg _ ident) = pPrint ident

data LLVMIR = Ret (Maybe LLVMValue)
            | BrCond LLVMValue LLVMLabel LLVMLabel
            | Br LLVMLabel
            | Add LLVMValue LLVMValue LLVMValue
            | Sub LLVMValue LLVMValue LLVMValue
            | Mul LLVMValue LLVMValue LLVMValue
            | SDiv LLVMValue LLVMValue LLVMValue
            | SRem LLVMValue LLVMValue LLVMValue
            | Alloca LLVMValue LLVMType
            | Store LLVMValue LLVMValue
            | Load LLVMValue LLVMValue
            | Call LLVMValue LLVMFuncIdent [LLVMValue]
            | ICmp CmpOp LLVMValue LLVMValue LLVMValue
            | And LLVMValue LLVMValue LLVMValue
            | Or LLVMValue LLVMValue LLVMValue
            | Xor LLVMValue LLVMValue LLVMValue
            deriving (Show, Eq)

data CmpOp = Eq
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
getType (LLVMReg t _ ) = t

labelAsIdent :: LLVMLabel -> LLVMIdent
labelAsIdent (LLVMLabel l) = LLVMIdent l

instance Pretty LLVMIR where
    pPrint (Ret Nothing) = text "ret" <+> text "void"
    pPrint (Ret (Just val)) = text "ret" <+> (pPrint.getType) val <+> pPrint val
    pPrint (Alloca res typ) = pPrint res <+> char '=' <+> text "alloca" <+> pPrint typ
    pPrint (Br label) = text "br" <+> text "label" <+> (pPrint.labelAsIdent) label
    pPrint (BrCond v label label') = text "br"
                                 <+> printType v
                                 <+> pPrint v
                                 <> char ','
                                 <+> text "label" <+> (pPrint.labelAsIdent) label
                                 <> char ','
                                 <+> text "label" <+> (pPrint.labelAsIdent) label'
    pPrint (Add r v v') = binOp "add" r v v'
    pPrint (Sub r v v') = binOp "sub" r v v'
    pPrint (SDiv r v v') = binOp "sdiv" r v v'
    pPrint (SRem r v v') = binOp "srem" r v v'
    pPrint (Store v vp) = text "store" <+> printWithType v <> char ',' <+> printWithType vp
    pPrint (Load r vp) = pPrint r <+> char '=' <+> text "load" <+>
                         printType r <> char ',' <+> printWithType vp
    pPrint (ICmp op r c c') = pPrint r <+> char '=' <+> text "icmp" <+> pPrint op <+>
                              printWithType c <> char ',' <+> pPrint c'
    pPrint (And r v v') = binOp "and" r v v'
    pPrint (Or r v v') = binOp "or" r v v'
    pPrint (Xor r v v') = binOp "xor" r v v'


printType :: LLVMValue -> Doc
printType = pPrint.getType

printWithType :: LLVMValue -> Doc
printWithType v = printType v <+> pPrint v

binOp :: String -> LLVMValue -> LLVMValue -> LLVMValue -> Doc
binOp s r v v' = pPrint r <+> char '=' <+> text s <+> printType r <+> pPrint v <> char ',' <+> pPrint v'

data LLVMBlock = LLVMBlock
                { _label :: LLVMLabel
                , _insns :: [LLVMIR]
                } deriving (Show, Eq)

instance Pretty LLVMBlock where
    pPrint block = pPrint (_label block) <> char ':' $+$
                   (indent . vcat) (pPrint <$> (_insns block))
        where indent = nest 4


data LLVMFuncDef = LLVMFuncDef LLVMType LLVMFuncIdent [LLVMValue] deriving (Eq, Show)

data LLVMFunction = LLVMFunction
                  { _definition :: LLVMFuncDef
                  , _fblocks :: [LLVMBlock]
                  , _entry :: LLVMBlock
                  , _exit :: LLVMBlock
                } deriving (Show, Eq)

instance Pretty LLVMFunction where
    pPrint func = header $+$ pPrint (_entry func) $+$ (vcat $ pPrint <$> (_fblocks func)) $+$ pPrint (_exit func)
        where (LLVMFuncDef rtype name args) = _definition func
              header = text "define" <+> pPrint rtype <+> char '@' <> pPrint name
                     <> parens (hsep $ punctuate comma (pPrint <$> args)) <> char '{'


data LLVMModule = LLVMModule
                { _functions :: [LLVMFunction]
                } deriving (Show, Eq)

data CodegenState = CodegenState
                  { _ast :: AST.TopDef
                  , _nextLabel :: Integer
                  , _nextIdent :: Integer
                  , _blocks :: M.Map LLVMLabel [LLVMIR]
                  , _localVars :: [(LLVMValue)]
                  }

data CodegenEnv = CodegenEnv
                { _varMap :: M.Map AST.Ident LLVMValue
                , _currentBlock :: LLVMLabel
                }


type CodegenM =  ReaderT CodegenEnv (StateT CodegenState (Except CompileError))

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
    where  newLabel l = ("label_" `T.append`
                           T.pack(show l))

newIdent :: CodegenM LLVMIdent
newIdent = do
    ident <- gets _nextIdent
    modify (\s -> s {_nextIdent = ident + 1})
    return $ (LLVMIdent . newIdent) ident
    where  newIdent l = (T.pack(show l))

getAddr :: AST.Ident -> CodegenM LLVMValue
getAddr name = do
    r <- asks ((M.lookup name)  . _varMap)
    case r of
      Just val -> return val
      Nothing -> throwError CodegenError

allocReg :: LLVMType -> CodegenM LLVMValue
allocReg typ = do
    id <- newIdent
    return (LLVMReg typ id)

allocConst :: Integer -> CodegenM LLVMValue
allocConst n = return (LLVMConst I32 n)

allocBool :: Bool -> CodegenM LLVMValue
allocBool v = return (LLVMConst I1 n)
    where n = if v then 1 else 0

allocLocalVar :: LLVMType -> CodegenM LLVMValue
allocLocalVar t = do
    r <- allocReg (Ptr t)
    modify (\s -> let newVars = r:(_localVars s) in s {_localVars = newVars})
    return r

emit :: LLVMIR -> CodegenM ()
emit ins = do
    block <- asks _currentBlock
    modify (\s -> let newBlock = ins:((_blocks s) M.! block)
                      in s {_blocks = M.insert block newBlock (_blocks s)})
    return ()

load :: AST.Ident -> CodegenM LLVMValue
load name = do
    addr <- getAddr name
    res <- allocReg ((derefType.getType) addr)
    emit $ Load res addr
    return res
    where derefType :: LLVMType -> LLVMType
          derefType (Ptr t) = t



compileExpr :: AST.Expr -> CodegenM LLVMValue
compileExpr (AST.Var name) = load name
compileExpr (AST.LitInt n) = allocConst n
compileExpr (AST.LitString txt) = undefined
compileExpr AST.LitTrue = allocBool True
compileExpr AST.LitFalse = allocBool False
compileExpr (AST.Call ident exprs) = do
    res <- mapM compileExpr exprs
    output <- allocReg I32
    emit $ Call output (LLVMFuncIdent ident) res -- TODO
    return output
compileExpr (AST.Neg exp) = compileExpr (AST.Add AST.Minus (AST.LitInt 0) exp)
compileExpr (AST.Not exp) = do
    e <- compileExpr exp
    r <- allocReg I1
    c <- allocConst 1
    emit $ Xor r e c
    return r
compileExpr (AST.Mul op exp exp') = undefined
compileExpr (AST.Add op exp exp') = do
    e  <- compileExpr exp
    e' <- compileExpr exp'
    r  <- allocReg (getType e)
    case op of
      AST.Plus -> do { emit $ Add r e e'; return r }
      AST.Minus -> do { emit $ Sub r e e'; return r }
compileExpr (AST.Comp rop exp exp') = undefined
compileExpr (AST.And exp exp') = undefined
compileExpr (AST.Or exp exp') = undefined

store :: LLVMValue -> LLVMValue -> CodegenM ()
store val addr = emit $ Store val addr

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
    mapM_ (\(val,addr) -> store val addr) (zip results locs)
    env <- ask
    let newVars = M.fromList (zip idents locs)
    return $ env { _varMap = (M.union newVars (_varMap env)) }
    where llvmTypeMap = M.fromList [ (AST.TInteger, I32)
                                   , (AST.TBool, I1)
                                   , (AST.TVoid, Void) ]
compileStmt (AST.Ret Nothing) = do { emit $ Ret Nothing; nop }
compileStmt (AST.Ret (Just exp)) = do
    r <- compileExpr exp
    emit $ Ret (Just r)
    nop
compileStmt (AST.Loop exp stmt) = do
    condBlock <- newBlock
    bodyBlock <- newBlock
    postBlock <- newBlock
    emit $ Br condBlock
    local (setCurrentBlock condBlock) (do
        r <- compileExpr exp
        emit $ BrCond r bodyBlock postBlock
        )
    local (setCurrentBlock bodyBlock) (do
        env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br condBlock)
        )
    env <- ask
    return (setCurrentBlock postBlock env)
compileStmt (AST.Block stmts) = do
    env <- ask
    env' <- compileStatements stmts
    let lastBlock = _currentBlock env'
    return (setCurrentBlock lastBlock env)
compileStmt (AST.Incr var) = compileStmt (AST.Ass var (AST.Add AST.Plus (AST.Var var) (AST.LitInt 1)))
compileStmt (AST.Decr var) = compileStmt (AST.Ass var (AST.Add AST.Minus (AST.Var var) (AST.LitInt 1)))
compileStmt (AST.If exp stmt) = do
    bodyBlock <- newBlock
    postBlock <- newBlock
    r <- compileExpr exp
    emit $ BrCond r bodyBlock postBlock
    local (setCurrentBlock bodyBlock) (do
        env' <- compileStmt stmt
        let lastBlock = _currentBlock env'
        local (setCurrentBlock lastBlock) (emit $ Br postBlock)
        )
    env <- ask
    return (setCurrentBlock postBlock env)


setCurrentBlock :: LLVMLabel -> CodegenEnv -> CodegenEnv
setCurrentBlock l env = env {_currentBlock = l}

nop :: CodegenM CodegenEnv
nop = do {env <- ask; return env}

compileStatements :: [AST.Stmt] -> CodegenM CodegenEnv
compileStatements stmts = compileAll stmts
  where runInEnv a e = local (const e) a
        compileAll :: [AST.Stmt] -> CodegenM CodegenEnv
        compileAll (s:ss) = do
            env <- compileStmt s
            local (const env) (compileAll ss)
        compileAll [] = nop





generateCode :: CodegenM LLVMFunction
generateCode = do
    (AST.TopDef _ _ _ stmts) <- gets _ast
    entry <- compileStatements stmts
    return (LLVMFunction {})
