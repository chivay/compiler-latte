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
            deriving (Show, Eq)

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
    pPrint (Mul r v v') = binOp "mul" r v v'
    pPrint (SDiv r v v') = binOp "sdiv" r v v'
    pPrint (SRem r v v') = binOp "srem" r v v'
    pPrint (Store v vp) = text "store" <+> printWithType v <> char ',' <+> printWithType vp
    pPrint (Load r vp) = pPrint r <+> char '=' <+> text "load" <+> printType r <> char ',' <+> printWithType vp

printType :: LLVMValue -> Doc
printType = pPrint.getType

printWithType :: LLVMValue -> Doc
printWithType v = printType v <+> pPrint v

binOp :: String -> LLVMValue -> LLVMValue -> LLVMValue -> Doc
binOp s r v v' = pPrint r <+> char '=' <+> printType r <+> pPrint v <> char ',' <+> pPrint v'

data LLVMBlock = LLVMBlock
                { _label :: LLVMLabel
                , _insns :: [LLVMIR]
                , _terminator :: LLVMIR
                } deriving (Show, Eq)

instance Pretty LLVMBlock where
    pPrint block = pPrint (_label block) <> char ':' $+$
                   (indent . vcat) (pPrint <$> (_insns block ++ [_terminator block]))
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

buildBlock :: [AST.Stmt] -> CodegenM ([AST.Stmt], [AST.Stmt])
buildBlock stmts = do
    let blockStmts = takeWhile (not . blockEnds) stmts
    let rest = dropWhile (not . blockEnds) stmts
    return (blockStmts, rest)
    where blockEnds :: AST.Stmt -> Bool
          blockEnds AST.Empty = False
          blockEnds (AST.Decl _ _ ) = False
          blockEnds (AST.Ass _ _ ) = False
          blockEnds (AST.Incr _) = False
          blockEnds (AST.Decr _) = False
          blockEnds (AST.ExpS _ ) = False
          blockEnds _ = True

compileStmt :: AST.Stmt -> CodegenM [LLVMIR]
compileStmt AST.Empty = undefined

compileBlock :: [AST.Stmt] -> CodegenM LLVMBlock
compileBlock stmts = undefined


compileBlocks :: [AST.Stmt] -> CodegenM LLVMLabel
compileBlocks stmts = do
    (current, others) <- buildBlock stmts
    this <- compileBlock current
    next <- compileBlocks others
    return (_label this)

generateCode :: CodegenM LLVMFunction
generateCode = do
    (AST.TopDef _ _ _ stmts) <- gets _ast
    entry <- compileBlocks stmts
    return (LLVMFunction {})
