{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Admiral.Operation (
    runOperation
  , ensureAcyclic
) where

import Control.DeepSeq
import Control.Monad.Reader
import Data.Graph
import Data.List
import qualified Data.Map as M
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
                   GraphOptions{..} -> audit
    exists <- doesFileExist master
    unless exists . throw $ MissingAdmiralFile master
    desc <- parseFromFileEx admiralP master
    case desc of
        Failure xs -> print xs
        Success m -> do
            let slist = ensureNoOrphans . ensureAcyclic . stronglyConnComp
                      $ map (\s -> (s, shipAlias s, map dependencySource $ shipDeps s)) m
                smap = M.fromList $ map (\s -> (shipAlias s, s)) slist
                stree = construct smap $ last slist
            stree `deepseq` runReaderT oper (Env stree slist)
    where construct m parent =
            Node parent [ construct m (m M.! node)
                        | node <- reverse $ map dependencySource (shipDeps parent) ]

ensureAcyclic :: [SCC Ship] -> [Ship]
ensureAcyclic (AcyclicSCC n:ns) = n:ensureAcyclic ns
ensureAcyclic (CyclicSCC ns:_) = throw $ CyclicDependencies ns
ensureAcyclic [] = []

ensureNoOrphans :: [Ship] -> [Ship]
ensureNoOrphans ss = case allDeps \\ allShips of
    [] -> ss
    (x:_) -> throw $ OrphanDependency x
    where allDeps = nub $ concatMap (map dependencySource . shipDeps) ss
          allShips = map shipAlias ss
