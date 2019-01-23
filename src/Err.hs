{-# LANGUAGE OverloadedStrings #-}

module Err where

import           Abs                            as AST
import qualified Data.Text                      as T
import           Prettify
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

data CompileError
  = TypeError T.Text
  | FunctionRedefinitionError T.Text
  | StructRedefinitionError T.Text
  | FieldRedefinitionError
  | RedefinitionError
  | UndefinedFunctionError AST.Ident
  | UndefinedVariableError AST.Ident
  | UndefinedType AST.Type
  | InvalidTypeError AST.Ident
  | ReturnPathError
  | UninitializedError
  | NoMethodError
  | UndefinedStructError
  | InvalidCastError T.Text
  | StmtLocatedError AST.Stmt
                     CompileError
  | CodegenError String
  deriving (Show)

instance Pretty CompileError where
  pPrint (StmtLocatedError stmt err) =
    text (show err) $+$ text "in statement:" $+$ (pPrint stmt)
  pPrint (FunctionRedefinitionError func) =
    text "Redefinition of function:" <+> (text . T.unpack) func
  pPrint err = text (show err)
