{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.Aeson.Prefix
-- Maintainer: Jiri Marsicek <jiri.marsicek@gmail.com>
--
-- Module provides 'flatten' and 'flatten'' which are used to flatten structure of 'Data.Aeson' JSON 'Value' as much as possible.
--
-- == Examples ('flatten')
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
--
-- == Examples ('flatten'')
--
-- === Basic objects
--
-- @{ "a": { "b": 1 } }@ results in @{ "a.b": 1 }@
--
-- @{ "a": { "b": { "c": 1 } } }@ results in @{ "a.b.c": 1 }@
--
-- @{ "a": [ { "b": { "c": 1 } }, { "b": { "c": 2 } } ] }@ results in @{ "a": [ { "b.c": 1 }, { "b.c": 2 } ] }@
--
-- === Name conflicts do not result in data loss
--
-- @{ "a": 1, "b": { "a": 2 } }@ results in @{ "a": 1, "b.a": 2 }@
module Data.Aeson.Flatten
    ( flatten
    , flatten'
    -- * Utility functions
    , isObject
    , mergeTo
    , tagKeys
    ) where

import           Data.Aeson          (Object, Value (..))
import           Data.Foldable       (foldl')
import qualified Data.HashMap.Strict as Map (elems, filter, fromList,
                                             mapWithKey, toList, union)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)

-- |
-- Predicate for testing whether provided 'Value' is a 'Object'.
isObject :: Value -> Bool
isObject (Object _) = True
isObject _          = False

-- |
-- Merges keys from second 'Value' to first 'Value'.
-- In case of name conflict, values from first are preserved.
mergeTo :: Value -> Value -> Value
mergeTo (Object o1) (Object o2) = Object $ Map.union o1 o2
mergeTo o _                     = o

-- |
-- Recursively Flattens the structure of the provided JSON 'Value'
flatten :: Value -> Value
flatten (Array a)  = Array $ flatten <$> a
flatten (Object o) = let flat = flatten <$> o
                         rest = Map.filter (not . isObject) flat
                     in foldl' mergeTo (Object rest) $ Map.elems flat
flatten v = v

-- |
-- Recursively Flattens the structure of the provided JSON 'Value'.
--
-- The key of each resulting key/value pair represents the path traversed to reach it in
-- the initial structure.
--
-- E.g. @"{ \"a\": { \"b\": 1 } }"@ results in @"{ \"a.b\": 1 }"@
flatten' :: Value -> Value
flatten' = flattenPreserveKeyPath ""

flattenPreserveKeyPath :: Text -> Value -> Value
flattenPreserveKeyPath k (Array a)  = Array $ flattenPreserveKeyPath k <$> a
flattenPreserveKeyPath k (Object o) =
  let flat = Map.mapWithKey (flattenPreserveKeyPath . tagKey k) o
      rest = tagKeys k $ Map.filter (not . isObject) flat
  in foldl' mergeTo (Object rest) $ Map.elems flat
flattenPreserveKeyPath _ v = v

-- |
-- Prepends text followed by a "." to all keys in the top level of the Map.
tagKeys :: Text -> Object -> Object
tagKeys "" o = o
tagKeys t o  =  Map.fromList $ map (\(k,v) -> (tagKey t k, v)) $ Map.toList o

tagKey :: Text -> Text -> Text
tagKey "" k = k
tagKey t k  = t <> "." <> k
