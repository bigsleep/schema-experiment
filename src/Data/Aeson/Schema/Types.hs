{-# LANGUAGE DeriveGeneric, StandaloneDeriving, KindSignatures #-}
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
, Tag
, Tag2
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
, tag2
) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Map.Strict (Map, fromList)
import Data.Aeson.Types (SumEncoding(..))

newtype Tag a = Tag ()
tag :: Tag a
tag = Tag ()

newtype Tag2 (a :: * -> *) = Tag2 ()
tag2 :: Tag2 a
tag2 = Tag2 ()


data Schema =
    Schema !SchemaValue |
    SchemaMaybe !Schema |
    SchemaUnion SumEncoding (Map Text Schema)
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

object :: [(Text, Schema)] -> SchemaValue
object = SchemaValueObject . SchemaObject . fromList

enum :: [Text] -> SchemaValue
enum = SchemaValueString . SchemaStringEnum

stringConstant :: Text -> SchemaValue
stringConstant = SchemaValueString . SchemaStringConstant

data SchemaNull = SchemaNull deriving (Show, Eq, Ord, Generic)

data SchemaBool = SchemaBool deriving (Show, Eq, Ord, Generic)

data SchemaNumber = SchemaNumber deriving (Show, Eq, Ord, Generic)

data SchemaString =
    SchemaString |
    SchemaStringConstant Text |
    SchemaStringEnum [Text]
    deriving (Show, Eq, Ord, Generic)

data SchemaArray =
    SchemaArray Schema |
    SchemaArrayTuple [Schema]
    deriving (Show, Eq, Ord, Generic)

data SchemaObject = SchemaObject (Map Text Schema) deriving (Show, Eq, Ord, Generic)

class HasSchema a where
    schema :: Tag a -> Schema

