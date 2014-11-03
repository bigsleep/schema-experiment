{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
module Data.Aeson.Schema.Classes
( HasSchema(..)
, GHasSchema(..)
, genericSchema
) where

import GHC.Generics (Generic(Rep))
import Data.Aeson.Types (Options, defaultOptions)
import Data.Aeson.Schema.Types (Tag, Schema, tag)

class HasSchema a where
    schema :: Tag a -> Schema
    default schema :: (Generic a, GHasSchema (Rep a)) => Tag a -> Schema
    schema = genericSchema defaultOptions

class GHasSchema f where
    gschema :: Options -> Tag (f a) -> Schema

genericSchema :: (Generic a, GHasSchema (Rep a)) => Options -> Tag a -> Schema
genericSchema opts = gschema opts . retag
    where
    retag :: Tag a -> Tag (Rep a ())
    retag _ = tag

