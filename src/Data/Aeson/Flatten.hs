{-# LANGUAGE CPP #-}
-- |
-- Module: Data.Aeson.Prefix
-- Maintainer: Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- Module provides 'flatten' which is used to flatten structure of 'Data.Aeson' JSON 'Value' as much as possible.
--
-- == Examples
--
-- === Basic objects
--
-- @{ "a": { "b": 1 } }@ results in @{ "b": 1 }@
--
-- @{ "a": { "b": { "c": 1 } } }@ results in @{ "c": 1 }@
--
-- @{ "a": [ { "b": { "c": 1 } }, { "b": { "c": 2 } } ] }@ results in @{ "a": [ { "c": 1 }, { "c": 2 } ] }@
--
-- === Name conflicts result in data loss
--
-- @{ "a": 1, "b": { "a": 2 } }@ results in @{ "a": 1 }@
--
-- * I don't know yet whether this is a feature or a bug :)
module Data.Aeson.Flatten
    ( flatten
    -- * Utility functions
    , isObject
    , mergeTo
    ) where

import Data.Aeson (Value(..))
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as Map (elems, filter, union)
#endif
import Data.Foldable (foldl')

-- |
-- Predicate for testing whether provided 'Value' is a 'Object'.
isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

-- |
-- Merges keys from second 'Value' to first 'Value'.
-- In case of name conflict, values from first are preserved.
mergeTo :: Value -> Value -> Value
#if MIN_VERSION_aeson(2,0,0)
mergeTo (Object o1) (Object o2) = Object $ KM.union o1 o2
#else
mergeTo (Object o1) (Object o2) = Object $ Map.union o1 o2
#endif
mergeTo o _ = o

-- |
-- Recursively Flattens the structure of the provided JSON 'Value'
flatten :: Value -> Value
flatten (Array a)  = Array $ flatten <$> a
#if MIN_VERSION_aeson(2,0,0)
flatten (Object o) = let flat = flatten <$> o
                         rest = KM.filter (not . isObject) flat
                      in foldl' mergeTo (Object rest) $ map snd $ KM.toList flat
#else
flatten (Object o) = let flat = flatten <$> o
                         rest = Map.filter (not . isObject) flat
                      in foldl' mergeTo (Object rest) $ Map.elems flat
#endif
flatten v = v
