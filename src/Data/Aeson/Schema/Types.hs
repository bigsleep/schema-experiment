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
, enum
, stringConstant
) where

import Prelude hiding (null)
import Data.Text (Text)
import Data.Map.Strict (Map, fromList)
import Data.Tagged (Tagged)

data Schema =
    Schema !SchemaValue |
    SchemaMaybe !Schema |
    SchemaSum (Map Text Schema)
    deriving (Show, Eq, Ord)

data SchemaValue =
    SchemaValueNull !SchemaNull |
    SchemaValueBool !SchemaBool |
    SchemaValueNumber !SchemaNumber |
    SchemaValueString !SchemaString |
    SchemaValueArray !SchemaArray |
    SchemaValueObject !SchemaObject
    deriving (Show, Eq, Ord)

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

enum :: [Text] -> SchemaValue
enum = SchemaValueString . SchemaStringEnum

stringConstant :: Text -> SchemaValue
stringConstant = SchemaValueString . SchemaStringConstant

data SchemaNull = SchemaNull deriving (Show, Eq, Ord)

data SchemaBool = SchemaBool deriving (Show, Eq, Ord)

data SchemaNumber = SchemaNumber deriving (Show, Eq, Ord)

data SchemaString =
    SchemaString |
    SchemaStringConstant Text |
    SchemaStringEnum [Text]
    deriving (Show, Eq, Ord)

data SchemaArray =
    SchemaArray Schema |
    SchemaArrayTuple [Schema]
    deriving (Show, Eq, Ord)

data SchemaObject = SchemaObject (Map Text Schema) deriving (Show, Eq, Ord)

class HasSchema a where
    schema :: Tagged a () -> Schema
