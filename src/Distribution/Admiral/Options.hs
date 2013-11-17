module Distribution.Admiral.Options (
    commands
  , SubOptions(..)
  , Options(..)
  , module Options.Applicative
) where

import Options.Applicative hiding ((&))

data Options = Options
             { admiralFile :: FilePath
             , allowDuplicates :: Bool
             , subOptions :: SubOptions
             } deriving Show

commands :: Parser Options
commands = Options
       <$> strOption
             ( long "master-file"
            <> short 'f'
            <> metavar "FILE"
            <> help "Use FILE instead of ./Admiralfile"
            <> value "./Admiralfile"
            <> completer (bashCompleter "file") )
       <*> flag False True
             ( long "allow-duplicates"
            <> help "Allow one image to depend on multiple versions of another" )
       <*> subparser
            ( command "sail" (info sailOptions
                $ progDesc "Build and deploy fleet")
           <> command "dock" (info dockOptions
                $ progDesc "Stop all containers in a fleet")
           <> command "sink" (info sinkOptions
                $ progDesc "Stop and destroy all containers in a fleet")
           <> command "audit" (info auditOptions
                $ progDesc "Print information about running and non-running containers in a fleet")
            )

data SubOptions = SailOptions
                | SinkOptions
                | DockOptions
                | AuditOptions
                deriving Show

sailOptions :: Parser SubOptions
sailOptions = pure SailOptions

sinkOptions :: Parser SubOptions
sinkOptions = pure SinkOptions

dockOptions :: Parser SubOptions
dockOptions = pure DockOptions

auditOptions :: Parser SubOptions
auditOptions = pure AuditOptions
