{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Admiral.Error (
    module Control.Exception
  , safely
  , AdmiralError(..)
  , MissingAdmiralFile(..)
  , CyclicDependencies(..)
  , OrphanDependency(..)
  , UnsourcedShip(..)
) where

import Control.Exception hiding (try)
import Data.List
import Data.Text (Text, unpack)
import Data.Typeable
import Distribution.Admiral.Ship
import System.IO
import System.Console.ANSI

data AdmiralError = forall e. Exception e => AdmiralError e
                  deriving Typeable

instance Show AdmiralError where show (AdmiralError e) = show e

instance Exception AdmiralError

admiralErrorToException :: Exception e => e -> SomeException
admiralErrorToException = toException . AdmiralError

admiralErrorFromException :: Exception e => SomeException -> Maybe e
admiralErrorFromException x = do
    AdmiralError a <- fromException x
    cast a

data MissingAdmiralFile = MissingAdmiralFile FilePath deriving Typeable

instance Show MissingAdmiralFile where
    show (MissingAdmiralFile f) = "Couldn't find Admiralfile at `" ++ f ++ "'"

instance Exception MissingAdmiralFile where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

data CyclicDependencies = CyclicDependencies [Ship] deriving Typeable

instance Show CyclicDependencies where
    show (CyclicDependencies ns) =
            "Cyclical dependencies! "
         ++ intercalate " <=> " (map (unpack . shipAlias) ns)

instance Exception CyclicDependencies where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

data OrphanDependency = OrphanDependency Text deriving Typeable

instance Show OrphanDependency where
    show (OrphanDependency d)
        = "`" ++ unpack d ++ "' was specified as a dependency, but was not found in the Admiralfile."

instance Exception OrphanDependency where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

data UnsourcedShip = UnsourcedShip Text deriving Typeable

instance Show UnsourcedShip where
    show (UnsourcedShip s) = "No source image was specified for the ship `" ++ unpack s ++ "'."

instance Exception UnsourcedShip where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

safely :: IO () -> IO ()
safely m = m `catch` \(e :: AdmiralError) -> do
    hSetSGR stderr [SetColor Foreground Dull Red, SetConsoleIntensity BoldIntensity]
    hPutStr stderr "Fatal"
    hSetSGR stderr []
    hPutStr stderr ": "
    hPrint stderr e
    hSetSGR stderr []
