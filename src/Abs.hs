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
  | TArray (Maybe Expr)
           Type
  deriving (Eq, Show)

data TypVar =
  TypVar Type
         Ident
  deriving (Eq, Show)

newtype Program =
  Program [TopDef]
  deriving (Eq, Show)

data TopDef
  = FuncDef Type
            Ident
            [TypVar]
            [Stmt]
  | StructDef Ident
              [TypVar]
  deriving (Eq, Show)

data LValue
  = Var Ident
  | Indexed Expr
            LValue
  | Field Ident
          LValue
  deriving (Eq, Show)

data Stmt
  = Empty
  | Block [Stmt]
  | Decl Type
         [DeclItem]
  | Ass LValue
        Expr
  | Incr LValue
  | Decr LValue
  | Ret (Maybe Expr)
  | If Expr
       Stmt
  | IfElse Expr
           Stmt
           Stmt
  | Loop Expr
         Stmt
  | Foreach TypVar
            Expr
            Stmt
  | ExpS Expr
  deriving (Eq, Show)

data DeclItem =
  DeclItem Ident
           (Maybe Expr)
  deriving (Eq, Show)

data Expr
  = Mem LValue
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
  | New Type
  deriving (Eq, Show)

data RelOp
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NEqual
  deriving (Eq, Show, Ord)

data MulOp
  = Times
  | Div
  | Mod
  deriving (Eq, Show)

data AddOp
  = Plus
  | Minus
  deriving (Eq, Show)
