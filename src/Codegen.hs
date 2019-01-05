{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import qualified Abs                  as AST
import qualified Data.Text            as T
import           Prelude              hiding ((<>))
import           Control.Monad.Except
import           Control.Monad.State
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass
import           Err

newtype LLVMLabel = LLVMLabel T.Text deriving (Show, Eq)
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
                } deriving (Show, Eq)

instance Pretty LLVMBlock where
    pPrint block = pPrint (_label block) <> char ':' $+$ (indent . vcat) (pPrint <$> (_insns block))
        where indent = nest 4


data LLVMFuncDef = LLVMFuncDef LLVMType LLVMFuncIdent [LLVMValue] deriving (Eq, Show)

data LLVMFunction = LLVMFunction
                  { _definition :: LLVMFuncDef
                  , _blocks :: [LLVMBlock]
                  , _entry :: LLVMBlock
                  , _exit :: LLVMBlock
                } deriving (Show, Eq)

instance Pretty LLVMFunction where
    pPrint func = header $+$ pPrint (_entry func) $+$ (vcat $ pPrint <$> (_blocks func)) $+$ pPrint (_exit func)
        where (LLVMFuncDef rtype name args) = _definition func
              header = text "define" <+> pPrint rtype <+> char '@' <> pPrint name
                     <> parens (hsep $ punctuate comma (pPrint <$> args)) <> char '{'


data LLVMModule = LLVMModule
                { _functions :: [LLVMFunction]
                } deriving (Show, Eq)

data CodegenState = CodegenState {
                    _ast :: AST.TopDef
                  }

type CodegenM = StateT CodegenState (Except CompileError)


generateCode :: CodegenM LLVMFunction
generateCode = do
    return (LLVMFunction {})
