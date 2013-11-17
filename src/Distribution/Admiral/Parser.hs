{-# LANGUAGE OverloadedStrings #-}

module Distribution.Admiral.Parser (
    Ship(..)
  , Image(..)
  , Dependency(..)
  , admiralP
  , module Text.Trifecta
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.Text as T
import Data.Text (Text, pack)
import Debug.Trace
import Text.Trifecta

data Ship = Ship
          { shipAlias :: Text
          , shipSource :: Image
          , shipCount :: Integer
          , shipDeps :: [Dependency]
          } deriving Show

data Image = Image
           { imageHost :: Maybe Text
           , imageNamespace :: Maybe Text
           , imageName :: Text
           } deriving Show

data Dependency = Global { dependencySource :: Text }
                | Local  { dependencySource :: Text }
                deriving Show

data Attribute = Source Image
               | Depends Bool {- global = True, local = False -} Text
               deriving Show

admiralP :: Parser [Ship]
admiralP = fmap concat (some shipP <* eof) where
    shipP = do
        names <- aliasP `sepBy1` mySymbolic ','
        char '\n'
        attrs <- some attributeP
        many (char '\n')
        return $ map (\s -> addAttrs (concat attrs) $ Ship s undefined 1 []) names
    aliasP = T.pack
          <$> ((:) <$> (lower <|> char '_')
                   <*> many (lower <|> digit <|> oneOf "_-"))
    attributeP = do
        some mostSpaces
        s <- string "source" <|> string "count" <|> string "depends" <|> string "has"
        some mostSpaces
        m <- case s of
            "source" -> return . Source <$> imageP
            "depends" -> map (Depends True) <$> aliasP `sepBy1` mySymbolic ','
            "has" -> map (Depends False) <$> aliasP `sepBy1` mySymbolic ','
        char '\n'
        return m
    imageP = Image <$> optional (try $ hostP <* char '/')
                   <*> optional (try $ namespaceP <* char '/')
                   <*> nameP
    hostP = T.intercalate "." . map T.pack <$> some (letter <|> digit <|> char '-') `sepBy2` char '.'
    namespaceP = T.pack <$> some (letter <|> digit <|> char '_')
    nameP = T.pack <$> some (letter <|> digit <|> oneOf "-_.")
    sepBy2 m sep = (:) <$> (m <* sep) <*> sepBy1 m sep
    addAttrs (Source im:as) ship = addAttrs as $ ship { shipSource = im }
    addAttrs (Depends True s:as) ship =
        addAttrs as $ ship { shipDeps = Global s:shipDeps ship }
    addAttrs (Depends False s:as) ship =
        addAttrs as $ ship { shipDeps = Local s:shipDeps ship }
    addAttrs [] s = s
    mostSpaces = oneOf "\t\v\f\r \160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"
    mySymbolic c = many mostSpaces *> char c <* many mostSpaces
