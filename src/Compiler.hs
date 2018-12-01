-- {-# LANGUAGE OverloadedStrings #-}
module Compiler where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text            as T

type CompilerM = ExceptT String (StateT CompilerState IO)

data CompilerState = CompilerState
  { _filename :: FilePath
  , _src      :: T.Text
  } deriving (Eq, Show)
