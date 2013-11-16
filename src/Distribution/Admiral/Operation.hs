{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Admiral.Operation (
    runOperation
  , ensureAcyclic
) where

import Control.Monad.Reader
import Data.Function
import Data.Graph
import Data.List
import Data.Text (pack)
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
            let deduped = deduplicate m
                sccs = stronglyConnComp $ map (\c@(Image n _ deps) -> (c, n, map fst deps)) deduped
                nodes = ensureUniqueDeps (allowDuplicates opts) . ensureUniqueAliases $ ensureAcyclic sccs
            runReaderT oper $ Env nodes

deduplicate :: [Image] -> [Image]
deduplicate is = go groups
    where groups = groupBy ((==) `on` imageName) is
          number ims@(Image n _ _:_) = reverse $
              zipWith (\num img -> img { imageNameOverride = Just $ n <> pack (show num) })
                      ([0..] :: [Integer])
                      ims
          number [] = error "impossibru!"
          go (x:xs) = if length x > 1
                          then number x ++ go xs
                          else x ++ go xs
          go [] = []

ensureAcyclic :: [SCC Image] -> [Image]
ensureAcyclic (AcyclicSCC n:ns) = n:ensureAcyclic ns
ensureAcyclic (CyclicSCC ns:_) = throw $ CyclicDependencies ns
ensureAcyclic [] = []

ensureUniqueAliases :: [Image] -> [Image]
ensureUniqueAliases (i@(Image x _ ys):is) =
    if all ((== 1) . length) groups
        then i:ensureUniqueAliases is
        else throw $ DuplicateAliases x (filter ((> 1) . length) groups)
    where groups = groupBy ((==) `on` snd) ys
ensureUniqueAliases [] = []

ensureUniqueDeps :: Bool -> [Image] -> [Image]
ensureUniqueDeps _ [] = []
ensureUniqueDeps True x = x
ensureUniqueDeps False (i@(Image n _ ds):xs) =
    if all ((== 1) . length) groups
        then i:ensureUniqueDeps False xs
        else throw $ DuplicateDependencies n
                (map (fst . head) $ filter ((> 1) . length) groups)
    where groups = groupBy ((==) `on` fst) ds
