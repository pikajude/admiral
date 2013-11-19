{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Distribution.Admiral.Parser (
    Ship(..)
  , Image(..)
  , Dependency(..)
  , admiralP
  , module Text.Trifecta
) where

import Control.Applicative
import qualified Data.Text as T
import Distribution.Admiral.Error
import Distribution.Admiral.Ship
import Text.Trifecta

admiralP :: Parser [Ship]
admiralP = fmap concat (some shipP <* eof) where
    shipP = do
        names <- aliasP `sepBy1` mySymbolic ','
        char '\n'
        attrs <- some attributeP
        many (char '\n')
        return $ map (\s -> addAttrs (concat attrs)
               $ Ship s (throw $ UnsourcedShip s) []) names
    aliasP = T.pack
          <$> ((:) <$> (lower <|> char '_')
                   <*> many (lower <|> digit <|> oneOf "_-"))
    attributeP = do
        some mostSpaces
        s <- string "source" <|> string "depends" <|> string "from"
        some mostSpaces
        m <- case s of
            "source" -> return . Source <$> imageP
            "from" -> return . From <$> imageP
            "depends" -> map Depends <$> aliasP `sepBy1` mySymbolic ','
            _ -> error "Cosmic rays!"
        char '\n'
        return m
    imageP = Image <$> optional (try $ hostP <* char '/')
                   <*> optional (try $ namespaceP <* char '/')
                   <*> nameP
    hostP = T.intercalate "." . map T.pack <$> some (letter <|> digit <|> char '-') `sepBy2` char '.'
    namespaceP = T.pack <$> some (letter <|> digit <|> char '_')
    nameP = T.pack <$> some (letter <|> digit <|> oneOf "-_.")
    sepBy2 m sep = (:) <$> (m <* sep) <*> sepBy1 m sep
    addAttrs (Source im:as) ship = addAttrs as $ ship { shipSource = LocalSource im }
    addAttrs (From im:as) ship = addAttrs as $ ship { shipSource = RemoteSource im }
    addAttrs (Depends s:as) ship =
        addAttrs as $ ship { shipDeps = Dependency s:shipDeps ship }
    addAttrs [] s = s
    mostSpaces = oneOf "\t\v\f\r \160\5760\6158\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"
    mySymbolic c = many mostSpaces *> char c <* many mostSpaces
