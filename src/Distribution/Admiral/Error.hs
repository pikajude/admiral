{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Admiral.Error (
    module Control.Exception
  , safely
  , AdmiralError(..)
  , MissingAdmiralFile(..)
  , CyclicDependencies(..)
  , DuplicateAliases(..)
  , DuplicateDependencies(..)
) where

import Control.Exception
import Data.List
import Data.Text (Text, unpack)
import Data.Typeable
import Distribution.Admiral.Parser
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

data CyclicDependencies = CyclicDependencies [Image] deriving Typeable

instance Show CyclicDependencies where
    show (CyclicDependencies ns) =
            "Cyclical dependencies! "
         ++ intercalate " <=> " (map (unpack . imageName) ns)

instance Exception CyclicDependencies where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

data DuplicateAliases = DuplicateAliases Text [[(Text,Text)]] deriving Typeable

instance Show DuplicateAliases where
    show (DuplicateAliases a ns) = "Duplicate aliases on "
                                ++ unpack a ++ "!\n  "
                                ++ intercalate "\n  " (map display ns)
        where display xs = intercalate ", " (map (unpack . fst) xs)
                        ++ " => " ++ unpack (snd $ head xs)

instance Exception DuplicateAliases where
    toException = admiralErrorToException
    fromException = admiralErrorFromException

data DuplicateDependencies = DuplicateDependencies Text [Text] deriving Typeable

instance Show DuplicateDependencies where
    show (DuplicateDependencies a ns) = unpack a
                                     ++ " expects multiple instances of "
                                     ++ intercalate ", " (map unpack ns)
                                     ++ ".\nUse --allow-duplicates if you really want to do this."

instance Exception DuplicateDependencies where
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
