{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Data.Aeson.Schema.Types
( Schema(..)
, SchemaValue(..)
, SchemaNull(..)
, SchemaBool(..)
, SchemaNumber(..)
, SchemaString(..)
, SchemaArray(..)
, SchemaObject(..)
, Tag
, null
, bool
, number
, string
, array
, object
, tuple
, enum
, stringConstant
, tag
, isObject
, isUnion
) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as M (Map, fromList)
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import qualified Data.Aeson.Types as DA (Options(..), defaultOptions, SumEncoding(..))
import Data.Aeson.Types (SumEncoding(..))

newtype Tag a = Tag ()
tag :: Tag a
tag = Tag ()

data Schema =
    Schema !SchemaValue |
    SchemaMaybe !Schema |
    SchemaUnion SumEncoding (M.Map T.Text Schema) |
    SchemaAlias T.Text
    deriving (Show, Eq, Ord, Generic)

deriving instance Show SumEncoding
deriving instance Eq SumEncoding
deriving instance Ord SumEncoding

data SchemaValue =
    SchemaValueNull !SchemaNull |
    SchemaValueBool !SchemaBool |
    SchemaValueNumber !SchemaNumber |
    SchemaValueString !SchemaString |
    SchemaValueArray !SchemaArray |
    SchemaValueObject !SchemaObject
    deriving (Show, Eq, Ord, Generic)

null, bool, number, string :: SchemaValue
null = SchemaValueNull SchemaNull
bool = SchemaValueBool SchemaBool
number = SchemaValueNumber SchemaNumber
string = SchemaValueString SchemaString

array :: Schema -> SchemaValue
array = SchemaValueArray . SchemaArray

tuple :: [Schema] -> SchemaValue
tuple = SchemaValueArray . SchemaArrayTuple

object :: [(T.Text, Schema)] -> SchemaValue
object = SchemaValueObject . SchemaObject . M.fromList

enum :: [T.Text] -> SchemaValue
enum = SchemaValueString . SchemaStringEnum

stringConstant :: T.Text -> SchemaValue
stringConstant = SchemaValueString . SchemaStringConstant

data SchemaNull = SchemaNull deriving (Show, Eq, Ord, Generic)

data SchemaBool = SchemaBool deriving (Show, Eq, Ord, Generic)

data SchemaNumber = SchemaNumber deriving (Show, Eq, Ord, Generic)

data SchemaString =
    SchemaString |
    SchemaStringConstant T.Text |
    SchemaStringEnum [T.Text]
    deriving (Show, Eq, Ord, Generic)

data SchemaArray =
    SchemaArray Schema |
    SchemaArrayTuple [Schema]
    deriving (Show, Eq, Ord, Generic)

data SchemaObject = SchemaObject (M.Map T.Text Schema) deriving (Show, Eq, Ord, Generic)

isObject :: Schema -> Bool
isObject (Schema (SchemaValueObject _)) = True
isObject _ = False

isUnion :: Schema -> Bool
isUnion (SchemaUnion _ _) = True
isUnion _ = False

options :: DA.Options
options = DA.defaultOptions { DA.sumEncoding = DA.ObjectWithSingleField }

instance DA.ToJSON Schema where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaValue where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaNull where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaBool where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaNumber where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaString where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaArray where
    toJSON = DA.genericToJSON options

instance DA.ToJSON SchemaObject where
    toJSON = DA.genericToJSON options

instance DA.FromJSON Schema where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaValue where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaNull where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaBool where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaNumber where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaString where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaArray where
    parseJSON = DA.genericParseJSON options

instance DA.FromJSON SchemaObject where
    parseJSON = DA.genericParseJSON options

instance DA.ToJSON DA.SumEncoding

instance DA.FromJSON DA.SumEncoding

deriving instance Generic DA.SumEncoding
