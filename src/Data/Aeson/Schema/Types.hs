module Data.Aeson.Schema.Types
( Schema(..)
, SchemaValue(..)
, SchemaNull(..)
, SchemaBool(..)
, SchemaNumber(..)
, SchemaString(..)
, SchemaArray(..)
, SchemaObject(..)
, HasSchema(..)
, null
, bool
, number
, string
, array
, object
, tuple
) where

import Prelude hiding (null)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Tagged (Tagged)

data Schema =
    Schema !SchemaValue |
    SchemaMaybe !Schema |
    SchemaUnion (HashMap Text Schema)
    deriving (Show, Eq)

data SchemaValue =
    SchemaValueNull !SchemaNull |
    SchemaValueBool !SchemaBool |
    SchemaValueNumber !SchemaNumber |
    SchemaValueString !SchemaString |
    SchemaValueArray !SchemaArray |
    SchemaValueObject !SchemaObject
    deriving (Show, Eq)

null, bool, number, string :: SchemaValue
null = SchemaValueNull SchemaNull
bool = SchemaValueBool SchemaBool
number = SchemaValueNumber SchemaNumber
string = SchemaValueString SchemaString

array :: Schema -> SchemaValue
array = SchemaValueArray . SchemaArray

tuple :: [Schema] -> SchemaValue
tuple = SchemaValueArray . SchemaArrayTuple

object :: [(Text, Schema)] -> SchemaValue
object = SchemaValueObject . SchemaObject . fromList

data SchemaNull = SchemaNull deriving (Show, Eq)

data SchemaBool = SchemaBool deriving (Show, Eq)

data SchemaNumber = SchemaNumber deriving (Show, Eq)

data SchemaString = SchemaString deriving (Show, Eq)

data SchemaArray =
    SchemaArray Schema |
    SchemaArrayTuple [Schema]
    deriving (Show, Eq)

data SchemaObject = SchemaObject (HashMap Text Schema) deriving (Show, Eq)

class HasSchema a where
    schema :: Tagged a () -> Schema
