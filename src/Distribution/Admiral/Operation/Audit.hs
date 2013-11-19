{-# LANGUAGE OverloadedStrings #-}

module Distribution.Admiral.Operation.Audit where

import Data.Text (unpack)
import Distribution.Admiral.Operation.Class

audit :: AdmiralOp ()
audit = do
    tr <- fleetTree
    liftIO . putStrLn . drawTree $ fmap (unpack . shipAlias) tr
