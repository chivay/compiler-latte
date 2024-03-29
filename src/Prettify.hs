{-# LANGUAGE OverloadedStrings #-}

module Prettify where

import           Data.Text                      as T
import           Prelude                        hiding ((<>))
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

import           Abs

instance Pretty T.Text where
  pPrint ident = text $ T.unpack ident

instance Pretty TopDef where
  pPrint (FuncDef typ name args stmts) = blockPack funcHeader body
    where
      funcHeader = pPrint typ <+> pPrint name <> lparen <> header <> rparen
      header = hsep $ punctuate comma (pPrint <$> args)
      body = vcat $ pPrint <$> stmts
  pPrint (StructDef name fields) =
    blockPack
      (text "class" <+> pPrint name)
      (vcat $ (<> semi) <$> pPrint <$> fields)

instance Pretty Type where
  pPrint TInteger = "int"
  pPrint TString = "string"
  pPrint TBool = "boolean"
  pPrint TVoid = "void"
  pPrint (TArray (Just size) t) =
    pPrint t <> char '[' <> pPrint size <> char ']'
  pPrint (TArray Nothing t) = pPrint t <> brackets Text.PrettyPrint.empty
  pPrint (TStruct ident) = pPrint ident

instance Pretty LValue where
  pPrint (Var ident)        = pPrint ident
  pPrint (Indexed exp base) = pPrint base <> brackets (pPrint exp)
  pPrint (Field field base) = pPrint base <> char '.' <> pPrint field

instance Pretty Expr where
  pPrint (Mem lval) = pPrint lval
  pPrint Null = "null"
  pPrint (Cast to expr) = parens (pPrint to) <> pPrint expr
  pPrint (LitInt n) = pPrint n
  pPrint LitTrue = "true"
  pPrint LitFalse = "false"
  pPrint (Call name args) =
    pPrint name <> parens (hsep $ punctuate comma (pPrint <$> args))
  pPrint (New typ) = text "new" <+> pPrint typ
  pPrint (LitString str) = doubleQuotes $ pPrint str
  pPrint (Neg expr) = char '-' <> parens (pPrint expr)
  pPrint (Not expr) = char '!' <> parens (pPrint expr)
  pPrint (Mul Times exp exp') = parens $ pPrint exp <+> char '*' <+> pPrint exp'
  pPrint (Mul Div exp exp') = parens $ pPrint exp <+> char '/' <+> pPrint exp'
  pPrint (Mul Mod exp exp') = parens $ pPrint exp <+> char '%' <+> pPrint exp'
  pPrint (Add Plus exp exp') = parens $ pPrint exp <+> char '+' <+> pPrint exp'
  pPrint (Add Minus exp exp') = parens $ pPrint exp <+> char '-' <+> pPrint exp'
  pPrint (And exp exp') = parens $ pPrint exp <+> "&&" <+> pPrint exp'
  pPrint (Or exp exp') = parens $ pPrint exp <+> "||" <+> pPrint exp'
  pPrint (Comp relop exp exp') =
    parens $ pPrint exp <+> toDoc relop <+> pPrint exp'
    where
      toDoc Less         = "<"
      toDoc LessEqual    = "<="
      toDoc Greater      = ">"
      toDoc GreaterEqual = ">="
      toDoc Equal        = "=="
      toDoc NEqual       = "!="

instance Pretty TypVar where
  pPrint (TypVar typ ident) = pPrint typ <+> pPrint ident

instance Pretty DeclItem where
  pPrint (DeclItem ident Nothing) = pPrint ident
  pPrint (DeclItem ident (Just exp)) = pPrint ident <+> char '=' <+> pPrint exp

instance Pretty Stmt where
  pPrint Empty = Text.PrettyPrint.empty
  pPrint (Block []) = lbrace <+> rbrace
  pPrint (Block stmts) =
    blockPack Text.PrettyPrint.empty ((indent . vcat) (pPrint <$> stmts))
  pPrint (Decl typ items) =
    pPrint typ <+> hsep (punctuate comma (pPrint <$> items)) <> semi
  pPrint (Ass ident exp) = pPrint ident <+> char '=' <+> pPrint exp <> semi
  pPrint (ExpS exp) = pPrint exp <> semi
  pPrint (Loop exp (Block stmts)) =
    blockPack ("while" <+> parens (pPrint exp)) (vcat $ pPrint <$> stmts)
  pPrint (Loop exp stmt) =
    blockPack ("while" <> parens (pPrint exp)) (pPrint stmt)
  pPrint (Incr ident) = pPrint ident <> "++" <> semi
  pPrint (Decr ident) = pPrint ident <> "--" <> semi
  pPrint (Ret Nothing) = "return" <> semi
  pPrint (Ret (Just exp)) = "return" <+> pPrint exp <> semi
  pPrint (If cond (Block stmts)) =
    blockPack ("if" <+> parens (pPrint cond)) (vcat $ pPrint <$> stmts)
  pPrint (If cond stmt) =
    blockPack ("if" <+> parens (pPrint cond)) (pPrint stmt)
  pPrint (IfElse cond (Block stmts) (Block stmts')) =
    blockPack ("if" <+> parens (pPrint cond)) (vcat $ pPrint <$> stmts) $+$
    blockPack ("else") (vcat $ pPrint <$> stmts')
  pPrint (IfElse cond stmt (Block stmts')) =
    blockPack ("if" <+> parens (pPrint cond)) (pPrint stmt) $+$
    blockPack ("else") (vcat $ pPrint <$> stmts')
  pPrint (IfElse cond (Block stmts) stmt) =
    blockPack ("if" <+> parens (pPrint cond)) (vcat $ pPrint <$> stmts) $+$
    blockPack ("else") (pPrint stmt)
  pPrint (IfElse cond stmt stmt') =
    blockPack ("if" <+> parens (pPrint cond)) (pPrint stmt) $+$
    blockPack ("else") (pPrint stmt')
  pPrint (Foreach tvar arr (Block stmt)) =
    blockPack
      (text "for" <+> parens (pPrint tvar <+> char ':' <+> pPrint arr))
      (pPrint stmt)
  pPrint (Foreach tvar arr stmt) =
    blockPack
      (text "for" <+> parens (pPrint tvar <+> char ':' <+> pPrint arr))
      (pPrint stmt)

instance Pretty Program where
  pPrint (Program tlds) = vcat $ pPrint <$> tlds

indent :: Doc -> Doc
indent = nest 4

blockPack :: Doc -> Doc -> Doc
blockPack header body = header <+> lbrace $+$ indent body $+$ rbrace
