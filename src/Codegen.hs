{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import qualified Data.Text            as T

type LLVMLabel = T.Text
type LLVMIdent = T.Text

data LLVMType = I32
              | Ptr LLVMType
              deriving (Show, Eq)

data LLVMValue = LLVMConst LLVMType Integer
               | LLVMReg   LLVMType LLVMIdent
               deriving (Show, Eq)

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
            | Call LLVMValue
            deriving (Show, Eq)

data LLVMBlock = LLVMBlock
                { _label :: LLVMLabel
                , _insns :: [LLVMIR]
                }


data LLVMFunction = LLVMFunction
                  { _name :: T.Text
                  , _blocks :: [LLVMBlock]
                  , _entry :: LLVMBlock
                  , _exit :: LLVMBlock
                  }

data LLVMModule = LLVMModule
                { _functions :: [LLVMFunction]
                }
