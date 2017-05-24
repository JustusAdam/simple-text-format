-- |
-- Module      : $Header$
-- Description : Simple format strings with named identifiers.
-- Copyright   : (c) Justus Adam 2017. All Rights Reserved.
-- License     : BSD3
-- Maintainer  : dev@justus.science
-- Stability   : experimental
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Data.Text.Format.Simple
    ( format
    , format'
    , formatC
    , formatC'
    , SubMap
    , Compiled
    , Name
    ) where


import           Data.Either
import qualified Data.Foldable                    as F
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Format.Simple.Internal as I


-- | Resolver for names.
type SubMap = Name -> Maybe Text


-- | Like 'format' but with the string already compiled
formatC :: Compiled -> SubMap -> (Text, [Name])
formatC (Compiled parts) lookup = (T.concat $ rights results, lefts results)
  where
    results = map f parts
    f (String t)   = return t
    f (NamedVar n) = maybe (Left n) return $ lookup n


-- | Like 'format'' but with the string already compiled
formatC' :: Compiled -> SubMap -> Text
formatC' p = fst . formatC p


-- | Compile a string to a format string and substitute the identifiers from the provided function into it.
--
-- Fails if the string canno be parsed.
-- Retuns the rendered string and a list of names which were used in the string but could not be resolved by the function.
format :: Text -> SubMap -> Either String (Text, [Name])
format a m = do
    a' <- I.parse a
    return $ formatC a' m

-- | Compile a string to a format string and substitute the identifiers from the provided function into it.
--
-- All errors are ignored. Parse failure is thrown as an error and the missing names are discarded.
format' :: Text -> SubMap -> Text
format' a = either error fst . format a
