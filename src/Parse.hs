{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           Abs

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token
                                               as Token
import qualified Data.Text                     as T
import           Control.Monad

languageDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> oneOf ['_']
    , Token.reservedNames   = [ "if"
                              , "else"
                              , "while"
                              , "class"
                              , "string"
                              , "int"
                              , "boolean"
                              , "void"
                              , "return"
                              ]
    , Token.reservedOpNames = [ "+"
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
    typVoid = reserved "void" >> return TBool

stmt :: Parser Stmt
stmt   = do
    s <-    blockStmt
        <|> ifStmt
        <|> ifElseStmt
        <|> whileStmt
        <|> (do reserved "return"; e <- optionMaybe expr; semi; return $ Ret e)
        <|> (do s <- assStmt; whitespace; semi; return s)
        <|> (do s <- exprStmt; whitespace; semi; return s)
        <|> (do semi >> return Empty)
    return s

exprStmt :: Parser Stmt
exprStmt = do
    e <- expr
    return $ ExpS e

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
        , [ Infix (reservedOp "&&" >> return And) AssocLeft
          , Infix (reservedOp "||" >> return Or)  AssocLeft
          ]
        , [ Infix (reservedOp "<" >> return (Comp Less))          AssocNone
          , Infix (reservedOp "<=" >> return (Comp LessEqual))    AssocNone
          , Infix (reservedOp ">" >> return (Comp Greater))       AssocNone
          , Infix (reservedOp ">=" >> return (Comp GreaterEqual)) AssocNone
          , Infix (reservedOp "==" >> return (Comp Equal))        AssocNone
          , Infix (reservedOp "!=" >> return (Comp NEqual))       AssocNone
          ]
        ]
    terms = 
        parens expr
            <|> try functionCall
            <|> fmap LitInt         integer
            <|> fmap (LitString . T.pack) stringLiteral
            <|> fmap Var ident
            <|> (reserved "true" >> return LitTrue)
            <|> (reserved "false" >> return LitFalse)
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
    stm  <- stmt
    reserved "else"
    stm' <- stmt
    IfElse cond stm <$> stmt

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- parens expr
    Loop cond <$> stmt

assStmt :: Parser Stmt
assStmt = do
    var <- ident
    reserved "="
    Ass var <$> expr


varDecl :: Parser DeclItem
varDecl = withInit <|> noInit
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
    t     <- typ
    items <- commaSep varDecl
    return $ Decl t []

typVar :: Parser TypVar
typVar = do
    t <- typ
    TypVar t <$> ident

topDef :: Parser TopDef
topDef = do
    t     <- typ
    name  <- ident
    args  <- parens (commaSep typVar)
    (Block stmts) <- blockStmt
    return $ TopDef t name args stmts

programParser :: Parser Program
programParser = many topDef
