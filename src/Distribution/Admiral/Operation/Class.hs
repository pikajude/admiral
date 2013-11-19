module Distribution.Admiral.Operation.Class (
    module Control.Monad.Reader
  , module Data.Tree
  , Env(Env)
  , Image(..)
  , Dependency(..)
  , Ship(..)
  , AdmiralOp
  , bottomUp, topDown, fleetTree
) where

import Control.Monad.Reader
import Data.Tree
import Distribution.Admiral.Parser

data Env = Env
         { _fleetTree :: Tree Ship
         , _fleetList :: [Ship]
         } deriving Show

type AdmiralOp = ReaderT Env IO

bottomUp :: (Ship -> AdmiralOp ()) -> AdmiralOp ()
bottomUp f = asks _fleetList >>= mapM_ f

topDown :: (Ship -> AdmiralOp ()) -> AdmiralOp ()
topDown f = asks _fleetList >>= mapM_ f . reverse

fleetTree :: AdmiralOp (Tree Ship)
fleetTree = asks _fleetTree
