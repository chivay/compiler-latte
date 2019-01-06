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
  | RedefinitionError
  | UndefinedFunctionError AST.Ident
  | UndefinedVariableError AST.Ident
  | InvalidTypeError AST.Ident
  | ReturnPathError
  | StmtLocatedError AST.Stmt
                     CompileError
  | CodegenError
  deriving (Show)

instance Pretty CompileError where
  pPrint (StmtLocatedError stmt err) =
    text (show err) $+$ text "in statement:" $+$ (pPrint stmt)
  pPrint (FunctionRedefinitionError func) =
    text "Redefinition of function:" <+> (text . T.unpack) func
  pPrint err = text (show err)
