module Abs where

import qualified Data.Text as T

type Ident = T.Text

data Type
  = TInteger
  | TString
  | TBool
  | TVoid
  | TFunc Type
          [Type]
  deriving (Eq, Show, Ord)

data TypVar =
  TypVar Type
         Ident
  deriving (Eq, Show)

newtype Program =
  Program [TopDef]
  deriving (Eq, Show)

data TopDef =
  TopDef Type
         Ident
         [TypVar]
         [Stmt]
  deriving (Eq, Show)

data Stmt
  = Empty
  | Block [Stmt]
  | Decl Type
         [DeclItem]
  | Ass Ident
        Expr
  | Incr Ident
  | Decr Ident
  | Ret (Maybe Expr)
  | If Expr
       Stmt
  | IfElse Expr
           Stmt
           Stmt
  | Loop Expr
         Stmt
  | ExpS Expr
  deriving (Eq, Show)

data DeclItem =
  DeclItem Ident
           (Maybe Expr)
  deriving (Eq, Show)

data Expr
  = Var Ident
  | LitInt Integer
  | LitTrue
  | LitFalse
  | Call Ident
         [Expr]
  | LitString T.Text
  | Neg Expr
  | Not Expr
  | Mul MulOp
        Expr
        Expr
  | Add AddOp
        Expr
        Expr
  | Comp RelOp
         Expr
         Expr
  | And Expr
        Expr
  | Or Expr
       Expr
  deriving (Eq, Show)

data RelOp
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NEqual
  deriving (Eq, Show)

data MulOp
  = Times
  | Div
  | Mod
  deriving (Eq, Show)

data AddOp
  = Plus
  | Minus
  deriving (Eq, Show)
