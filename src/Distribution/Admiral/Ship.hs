{-# LANGUAGE TemplateHaskell #-}

module Distribution.Admiral.Ship where

import Control.DeepSeq.TH
import Data.Text (Text)

data Dependency = Dependency { dependencySource :: Text }
                deriving Show

data Image = Image
           { imageHost :: Maybe Text
           , imageNamespace :: Maybe Text
           , imageName :: Text
           } deriving Show

data Source = LocalSource Image
            | RemoteSource Image
            deriving Show

data Ship = Ship
          { shipAlias :: Text
          , shipSource :: Source
          , shipDeps :: [Dependency]
          } deriving Show

deriveNFData ''Image
deriveNFData ''Dependency
deriveNFData ''Source
deriveNFData ''Ship

data Attribute = Source Image
               | From Image
               | Depends Text
               deriving Show
