{-# LANGUAGE OverloadedStrings #-}

module Distribution.Admiral.Operation.Sail where

import Distribution.Admiral.Operation.Class

sail :: AdmiralOp ()
sail = topDown $ liftIO . print
