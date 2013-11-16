module Distribution.Admiral.Parser (
    Image(..)
  , admiralP
  , module Text.Trifecta
) where

import Control.Applicative
import Control.Monad
import Data.Text (Text, pack)
import Text.Trifecta

data Image = Image
           { imageName :: Text
           , imageNameOverride :: Maybe Text
           , imageDependencies :: [(Text, Text)]
           } deriving Show

admiralP :: Parser [Image]
admiralP = fmap fill $ some (Image
                         <$> (identifier <* noisyNl)
                         <*> pure Nothing
                         <*> many dependencyP) <* eof
    where fill cs = stubs ++ cs
           where stubs = map (\y -> Image y Nothing [])
                       . filter (\x -> all (\(Image q _ _) -> q /= x) cs)
                       $ concatMap (map fst . imageDependencies) cs

identifier :: Parser Text
identifier = fmap pack $ (:) <$> (lower <|> oneOf "_-") <*> many (lower <|> oneOf "_-" <|> digit)

dependencyP :: Parser (Text, Text)
dependencyP = do
    some (oneOf " \t")
    depName <- identifier
    spaces
    string "=>"
    spaces
    alias <- identifier
    noisyNl
    return (depName, alias)

noisyNl :: Parser ()
noisyNl = void $ many (oneOf " \t") >> some (char '\n')
