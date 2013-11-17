{-# LANGUAGE OverloadedStrings #-}

module Distribution.Admiral.Operation.Sail where

import Data.Monoid
import qualified Data.Text.IO as T
import Distribution.Admiral.Operation.Class

sail :: AdmiralOp ()
sail = bottomUp $ const (return ())
