{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Text.Format.Simple where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text as A
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.String
import Lens.Micro
import Lens.Micro.Internal
import Data.Hashable
import Data.Either
import GHC.Exts
import qualified Data.Foldable as F


newtype Name = Name { unwrapName :: Text } deriving (Eq, Hashable)


instance IsString Name where fromString = Name . fromString


data Part
    = String Text
    | NamedVar Name


newtype Compiled = Compiled { unCompiled :: [Part] }


instance IsList Compiled where
    type Item Compiled = Part
    fromList = Compiled
    toList = unCompiled


instance IsString Compiled where
    fromString = either error id . Data.Text.Format.Simple.parse . fromString


parse :: Text -> Either String Compiled
parse = parseOnly parser


parser :: Parser Compiled
parser = Compiled <$> partParser `manyTill` endOfInput


partParser :: Parser Part
partParser = NamedVar <$> parseVar <|> String <$> parseString



parseVar :: Parser Name
parseVar = Name <$> do
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


class Formattable a where
    compile :: a -> Either String Compiled

instance Formattable Compiled where compile = return
instance Formattable Text where compile = Data.Text.Format.Simple.parse
instance Foldable f => Formattable (f Part) where compile = return . Compiled . F.toList


substitutePrecompiled :: (At m, Index m ~ Name, IxValue m ~ Text) => Compiled -> m -> (Text, [Name])
substitutePrecompiled (Compiled parts) m = (T.concat $ rights results, lefts results)
  where
    results = map f parts
    f (String t) = return t
    f (NamedVar n) = maybe (Left n) return $ m ^. at n


substitutePrecompiled' :: (At m, Index m ~ Name, IxValue m ~ Text) => Compiled -> m -> Text
substitutePrecompiled' p = fst . substitutePrecompiled p


substitute :: (At m, Index m ~ Name, IxValue m ~ Text, Formattable a) => a -> m -> Either String (Text, [Name])
substitute a m = do
    a' <- compile a
    return $ substitutePrecompiled a' m

substitute' :: (At m, Index m ~ Name, IxValue m ~ Text, Formattable a) => a -> m -> Text
substitute' a = either error fst . substitute a
