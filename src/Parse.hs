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
        , "new"
        , "for"
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

lexeme = Token.lexeme lexer

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

braces = Token.braces lexer

brackets = Token.brackets lexer

integer = Token.integer lexer

semi = Token.semi lexer

whitespace = Token.whiteSpace lexer

commaSep = Token.commaSep lexer

semiSep = Token.semiSep lexer

stringLiteral = Token.stringLiteral lexer

ident :: Parser T.Text
ident = fmap T.pack identifier

typ :: Parser Type
typ = do
  t <- simpleType
  x <- many $ brackets $ return ()
  let wrapped = foldl (\t _ -> TArray Nothing t) t x
  return wrapped
  where
    typInteger = reserved "int" >> return TInteger
    typString = reserved "string" >> return TString
    typBool = reserved "boolean" >> return TBool
    typVoid = reserved "void" >> return TVoid
    simpleType = typInteger <|> typString <|> typBool <|> typVoid

initType :: Parser Type
initType = do
  t <- simpleType
  size <- brackets expr
  return (TArray (Just size) t)
  where
    typInteger = reserved "int" >> return TInteger
    typString = reserved "string" >> return TString
    typBool = reserved "boolean" >> return TBool
    typVoid = reserved "void" >> return TVoid
    simpleType = typInteger <|> typString <|> typBool <|> typVoid

lValue :: Parser LValue
lValue = do
  base <- ident
  cont <- (many . choice) [fieldAccess, indexedAccess]
  return $ foldl (\a i -> i a) (Var base) cont
  where
    fieldAccess = do
      lexeme $ char '.'
      field <- ident
      return (Field field)
    indexedAccess = do
      expr <- brackets expr
      return (Indexed expr)

stmt :: Parser Stmt
stmt = choice $ try <$> ([ blockStmt
              , ifElseStmt
              , ifStmt
              , whileStmt
              , foreachStmt
              , returnStmt
              , exprStmt
              , assStmt
              , declStmt
              , incStmt
              , decStmt
              ])

incStmt :: Parser Stmt
incStmt =
    (do var <- lValue
        reservedOp "++"
        semi
        return $ Incr var)

decStmt :: Parser Stmt
decStmt =
    (do var <- lValue
        reservedOp "--"
        semi
        return $ Decr var)

returnStmt :: Parser Stmt
returnStmt = do
  reserved "return"
  e <- optionMaybe expr
  semi
  return $ Ret e

exprStmt :: Parser Stmt
exprStmt =
  try
    (do e <- try expr
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
      (reserved "true" >> return LitTrue) <|>
      (reserved "false" >> return LitFalse) <|>
      (do reserved "new"
          t <- initType
          return (New t)) <|>
      fmap Mem lValue
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
  var <- lValue
  lexeme $ char '='
  exp <- expr
  semi
  return $ Ass var exp

varDecl :: Parser DeclItem
varDecl = try withInit <|> noInit
  where
    withInit = do
      name <- ident
      lexeme $ char '='
      exp <- expr
      return $ (DeclItem name . Just) exp
    noInit = do
      name <- ident
      return $ DeclItem name Nothing

declStmt :: Parser Stmt
declStmt = do
  t <- typ
  items <- commaSep varDecl
  semi
  return $ Decl t items

foreachStmt :: Parser Stmt
foreachStmt = do
    reserved "for"
    lexeme $ char '('
    iter <- typVar
    lexeme $ char ':'
    arr <- ident
    lexeme $ char ')'
    s <- stmt
    return $ Foreach iter arr s

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
