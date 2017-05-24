{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK not-home #-}
module Data.Text.Format.Simple.Internal where


import           Control.Applicative
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text       as A
import           Data.Either
import qualified Data.Foldable              as F
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Exts


-- | The name of a referenced variable.
type Name = Text


data Part
    = String Text
    | NamedVar Name
    deriving Eq

-- | A precompiled format string. Can be used to render several times.
newtype Compiled = Compiled { unCompiled :: [Part] } deriving Eq


instance Show Compiled where
    show = show . T.unpack . T.concat . map f . unCompiled
      where
        f (String s) = s
        f (NamedVar n) = "${" `mappend` n `mappend` "}"

instance IsString Compiled where
    fromString = either error id . Data.Text.Format.Simple.Internal.parse . fromString


parse :: Text -> Either String Compiled
parse = parseOnly parser


parser :: Parser Compiled
parser = Compiled <$> partParser `manyTill` endOfInput


partParser :: Parser Part
partParser = NamedVar <$> parseVar <|> String <$> parseString



parseVar :: Parser Name
parseVar = do
    char '$'
    char '{'
    str <- A.takeWhile (/= '}')
    char '}'
    return str


parseString :: Parser Text
parseString = do
    str <- A.takeWhile (/= '$')

    rest <- (lookAhead (string "${") >> return "")
            <|> (T.cons <$> char '$' <*> parseString)
            <|> return ""
    return $ str `mappend` rest

