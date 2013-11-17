module Distribution.Admiral.Operation.Class (
    module Control.Monad.Reader
  , module Data.Tree
  , Env(..)
  , Image(..)
  , Dependency(..)
  , AdmiralOp
  , bottomUp, topDown
) where

import Control.Monad.Reader
import Data.Tree
import Distribution.Admiral.Parser

data Env = Env { images :: Tree Image } deriving Show

type AdmiralOp = ReaderT Env IO

bottomUp :: (Image -> AdmiralOp ()) -> AdmiralOp ()
bottomUp f = asks images >>= mapM_ f . reverse . flatten

topDown :: (Image -> AdmiralOp ()) -> AdmiralOp ()
topDown f = asks images >>= mapM_ f . flatten
