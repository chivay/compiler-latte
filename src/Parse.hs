{-# LANGUAGE OverloadedStrings #-}

module Parse where

import           Control.Monad
import qualified Data.Text                              as T
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

import           Abs

languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> oneOf ['_']
    , Token.reservedNames =
        [ "if"
        , "else"
        , "while"
        , "class"
        , "string"
        , "int"
        , "boolean"
        , "void"
        , "return"
        ]
    , Token.reservedOpNames =
        [ "+"
        , "-"
        , "*"
        , "/"
        , "%"
        , "<"
        , ">"
        , "<="
        , ">="
        , "=="
        , "!="
        , "!"
        , "&&"
        , "||"
        , ","
        ]
    , Token.caseSensitive = True
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

braces = Token.braces lexer

integer = Token.integer lexer

semi = Token.semi lexer

whitespace = Token.whiteSpace lexer

commaSep = Token.commaSep lexer

semiSep = Token.semiSep lexer

stringLiteral = Token.stringLiteral lexer

ident :: Parser T.Text
ident = fmap T.pack identifier

typ :: Parser Type
typ = typInteger <|> typString <|> typBool <|> typVoid
  where
    typInteger = reserved "int" >> return TInteger
    typString = reserved "string" >> return TString
    typBool = reserved "boolean" >> return TBool
    typVoid = reserved "void" >> return TVoid

stmt :: Parser Stmt
stmt =
  whitespace >>
  (blockStmt <|> (try ifElseStmt) <|> ifStmt <|> whileStmt <|> returnStmt <|>
   exprStmt <|>
   assStmt <|>
   declStmt <|>
   incStmt <|>
   decStmt <|>
   (semi >> return Empty))

incStmt :: Parser Stmt
incStmt =
  try
    (do var <- ident
        reservedOp "++"
        return $ Incr var)

decStmt :: Parser Stmt
decStmt =
  try
    (do var <- ident
        reservedOp "--"
        return $ Incr var)

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  e <- optionMaybe expr
  whitespace
  semi
  return $ Ret e

exprStmt :: Parser Stmt
exprStmt =
  try
    (do e <- try expr
        whitespace
        semi
        return $ ExpS e)

blockStmt :: Parser Stmt
blockStmt = do
  stmts <- braces (many stmt)
  return $ Block stmts

expr :: Parser Expr
expr = buildExpressionParser operators terms
  where
    operators =
      [ [Prefix (reservedOp "-" >> return Neg)]
      , [ Infix (reservedOp "*" >> return (Mul Times)) AssocLeft
        , Infix (reservedOp "/" >> return (Mul Div)) AssocLeft
        , Infix (reservedOp "%" >> return (Mul Mod)) AssocLeft
        ]
      , [ Infix (reservedOp "+" >> return (Add Plus)) AssocLeft
        , Infix (reservedOp "-" >> return (Add Minus)) AssocLeft
        ]
      , [Prefix (reservedOp "!" >> return Not)]
      , [ Infix (reservedOp "<" >> return (Comp Less)) AssocNone
        , Infix (reservedOp "<=" >> return (Comp LessEqual)) AssocNone
        , Infix (reservedOp ">" >> return (Comp Greater)) AssocNone
        , Infix (reservedOp ">=" >> return (Comp GreaterEqual)) AssocNone
        , Infix (reservedOp "==" >> return (Comp Equal)) AssocNone
        , Infix (reservedOp "!=" >> return (Comp NEqual)) AssocNone
        ]
      , [ Infix (reservedOp "&&" >> return And) AssocLeft
        , Infix (reservedOp "||" >> return Or) AssocLeft
        ]
      ]
    terms =
      parens expr <|> try functionCall <|> fmap LitInt integer <|>
      fmap (LitString . T.pack) stringLiteral <|>
      fmap Var ident <|>
      (reserved "true" >> return LitTrue) <|>
      (reserved "false" >> return LitFalse)
    functionCall = do
      name <- ident
      args <- parens (commaSep expr)
      return $ Call name args

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens expr
  If cond <$> stmt

ifElseStmt :: Parser Stmt
ifElseStmt = do
  reserved "if"
  cond <- parens expr
  stm <- stmt
  reserved "else"
  IfElse cond stm <$> stmt

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- parens expr
  Loop cond <$> stmt

assStmt :: Parser Stmt
assStmt = do
  var <-
    try
      (do var <- ident
          reserved "="
          return var)
  Ass var <$> expr

varDecl :: Parser DeclItem
varDecl = (try withInit) <|> noInit
  where
    withInit = do
      name <- ident
      reserved "="
      DeclItem name . Just <$> expr
    noInit = do
      name <- ident
      return $ DeclItem name Nothing

declStmt :: Parser Stmt
declStmt = do
  t <- typ
  items <- commaSep varDecl
  return $ Decl t items

typVar :: Parser TypVar
typVar = do
  t <- typ
  TypVar t <$> ident

topDef :: Parser TopDef
topDef = do
  whitespace
  t <- typ
  name <- ident
  args <- parens (commaSep typVar)
  (Block stmts) <- blockStmt
  return $ TopDef t name args stmts

program :: Parser Program
program = Program <$> many topDef <* eof

parseSource :: FilePath -> IO (Either ParseError Program)
parseSource = parseFromFile program
