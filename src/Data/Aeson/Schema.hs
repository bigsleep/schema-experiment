module Data.Aeson.Schema
( schema
, module Data.Aeson.Schema.Types
, module Data.Aeson.Schema.Classes
) where

import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Classes
import Data.Aeson.Schema.Instances ()
import Data.Aeson.Schema.Generic ()

import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as M (Map, empty)

schema :: (HasSchema a) => Tag a -> (Schema, M.Map T.Text Schema)
schema = schemaJSON M.empty
