{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}
module Data.Aeson.Schema.Classes
( HasSchema(..)
, GHasSchema(..)
, genericSchema
) where

import GHC.Generics (Generic(Rep))
import Data.Aeson.Types (Options, defaultOptions)
import Data.Aeson.Schema.Types (Tag, Schema, tag)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.State.Strict (State, runState)

class HasSchema a where
    schema :: Map Text Schema -> Tag a -> (Schema, Map Text Schema)
    default schema :: (Generic a, GHasSchema (Rep a)) => Map Text Schema -> Tag a -> (Schema, Map Text Schema)
    schema = genericSchema defaultOptions

class GHasSchema f where
    gschema :: Options -> Tag (f a) -> State (Map Text Schema) Schema

genericSchema :: (Generic a, GHasSchema (Rep a)) => Options -> Map Text Schema -> Tag a -> (Schema, Map Text Schema)
genericSchema opts m = flip runState m . genericSchema' opts

genericSchema' :: (Generic a, GHasSchema (Rep a)) => Options -> Tag a -> State (Map Text Schema) Schema
genericSchema' opts = gschema opts . retag
    where
    retag :: Tag a -> Tag (Rep a ())
    retag _ = tag

