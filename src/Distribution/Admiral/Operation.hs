{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Admiral.Operation (
    runOperation
  , ensureAcyclic
) where

import Control.Arrow
import Control.Lens hiding (children)
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Function
import Data.Graph
import Data.Digest.Pure.MD5
import Data.List
import qualified Data.Map as M
import Data.Text.Encoding
import Data.Text.Lens
import Data.Text (Text, pack)
import Distribution.Admiral.Error
import Distribution.Admiral.Parser
import Distribution.Admiral.Options
import Distribution.Admiral.Operation.Class
import Distribution.Admiral.Operation.Audit
import Distribution.Admiral.Operation.Dock
import Distribution.Admiral.Operation.Sail
import Distribution.Admiral.Operation.Sink
import System.Directory

runOperation :: Options -> IO ()
runOperation opts = do
    let master = admiralFile opts
        oper = case subOptions opts of
                   SailOptions{..} -> sail
                   DockOptions{..} -> dock
                   SinkOptions{..} -> sink
                   AuditOptions{..} -> audit
    exists <- doesFileExist master
    unless exists . throw $ MissingAdmiralFile master
    desc <- parseFromFileEx admiralP master
    case desc of
        Failure xs -> print xs
        Success m -> do
            let sccs = ensureAcyclic . stronglyConnComp
                     $ map (\s -> (s, shipAlias s, map dependencySource $ shipDeps s)) m
            print sccs
            runReaderT oper $ Env undefined

hashText :: Text -> Text
hashText = pack . show . md5 . fromStrict . encodeUtf8

ensureAcyclic :: [SCC Ship] -> [Ship]
ensureAcyclic (AcyclicSCC n:ns) = n:ensureAcyclic ns
ensureAcyclic (CyclicSCC ns:_) = throw $ CyclicDependencies ns
ensureAcyclic [] = []
