module Distribution.Admiral.Options (
    commands
  , SubOptions(..)
  , Options(..)
  , module Options.Applicative
) where

import Options.Applicative hiding ((&))

data Options = Options
             { admiralFile :: FilePath
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
       <*> subparser
            ( command "sail" (info sailOptions
                $ progDesc "Build and deploy fleet")
           <> command "dock" (info dockOptions
                $ progDesc "Stop all containers in a fleet")
           <> command "sink" (info sinkOptions
                $ progDesc "Stop and destroy all containers in a fleet")
           <> command "graph" (info graphOptions
                $ progDesc "Show the Admiralfile's dependency graph")
           )

data SubOptions = SailOptions
                | SinkOptions
                | DockOptions
                | GraphOptions
                deriving Show

sailOptions :: Parser SubOptions
sailOptions = pure SailOptions

sinkOptions :: Parser SubOptions
sinkOptions = pure SinkOptions

dockOptions :: Parser SubOptions
dockOptions = pure DockOptions

graphOptions :: Parser SubOptions
graphOptions = pure GraphOptions
