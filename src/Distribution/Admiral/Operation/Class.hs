module Distribution.Admiral.Operation.Class (
    module Control.Monad.Reader
  , Env(..)
  , AdmiralOp
  , eachDependency
) where

import Control.Monad.Reader
import Distribution.Admiral.Parser

data Env = Env { imageList :: [Image] } deriving Show

type AdmiralOp = ReaderT Env IO

eachDependency :: (Image -> AdmiralOp ()) -> AdmiralOp ()
eachDependency f = asks imageList >>= mapM_ f
