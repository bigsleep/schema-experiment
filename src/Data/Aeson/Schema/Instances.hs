{-# LANGUAGE OverloadedStrings, FlexibleInstances, TemplateHaskell, OverlappingInstances #-}
module Data.Aeson.Schema.Instances
(
) where

import Data.Aeson.Schema.Types
import Data.Aeson.Schema.TH (tupleInstanceOfHasSchema)

import Control.Monad (mapM, liftM2)
import Data.Tagged (Tagged, retag)
import Data.Scientific (Scientific)
import Data.Fixed (HasResolution, Fixed)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Text (Text)
import Data.Ratio (Ratio)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

instance (HasSchema a) => HasSchema (Maybe a) where
    schema = SchemaMaybe . schema . etag

instance (HasSchema a, HasSchema b) => HasSchema (Either a b) where
    schema p = SchemaUnion . H.fromList $ [("Left", l), ("Right", r)]
        where
        l = schema . leftTag $ p
        r = schema . etag $ p

        leftTag :: Tagged (Either a b) () -> Tagged a ()
        leftTag = retag

instance HasSchema Bool where
    schema _ = Schema bool

instance HasSchema () where
    schema _ = Schema . tuple $ []

instance HasSchema [Char] where
    schema _ = Schema string

instance HasSchema Char where
    schema _ = Schema string

instance HasSchema Scientific where
    schema _ = Schema number

instance HasSchema Double where
    schema _ = Schema number

instance HasSchema Float where
    schema _ = Schema number

instance HasSchema (Ratio Integer) where
    schema _ = Schema . object $
        [ ("numerator", Schema number)
        , ("denominator", Schema number)
        ]

instance (HasResolution a) => HasSchema (Fixed a) where
    schema _ = Schema number

instance HasSchema Int where
    schema _ = Schema number

instance HasSchema Integer where
    schema _ = Schema number

instance HasSchema Int8 where
    schema _ = Schema number

instance HasSchema Int16 where
    schema _ = Schema number

instance HasSchema Int32 where
    schema _ = Schema number

instance HasSchema Int64 where
    schema _ = Schema number

instance HasSchema Word where
    schema _ = Schema number

instance HasSchema Word8 where
    schema _ = Schema number

instance HasSchema Word16 where
    schema _ = Schema number

instance HasSchema Word32 where
    schema _ = Schema number

instance HasSchema Word64 where
    schema _ = Schema number

instance HasSchema Text where
    schema _ = Schema string

instance HasSchema LT.Text where
    schema _ = Schema string

instance HasSchema a => HasSchema [a] where
    schema = Schema . array . schema . etag

instance HasSchema a => HasSchema (Set.Set a) where
    schema = Schema . array . schema . etag

instance HasSchema a => HasSchema (HashSet.HashSet a) where
    schema = Schema . array . schema . etag

instance HasSchema IntSet.IntSet where
    schema _ = Schema . array . Schema $ number

instance (HasSchema a) => HasSchema (IntMap.IntMap a) where
    schema t = Schema . array . Schema . tuple $ [Schema number, schema (etag t)]

etag :: Tagged (m a) () -> Tagged a ()
etag = retag

$(foldr (liftM2 (++)) (return []) $ map tupleInstanceOfHasSchema [2..10])

{-
-- TH使う?
instance (HasSchema a, HasSchema b) => HasSchema (a, b) where
    schema t = Schema . tuple $ [x, y]
        where
        x = schema $ e1Tag t
        y = schema $ e2Tag t

        e1Tag :: Tagged (a, b) () -> Tagged a ()
        e1Tag = retag

        e2Tag :: Tagged (a, b) () -> Tagged b ()
        e2Tag = retag

-- TH使う?
instance (HasSchema a, HasSchema b, HasSchema c) => HasSchema (a, b, c) where
    schema t = Schema . tuple $ [x, y, z]
        where
        x = schema $ e1Tag t
        y = schema $ e2Tag t
        z = schema $ e3Tag t

        e1Tag :: Tagged (a, b, c) () -> Tagged a ()
        e1Tag = retag

        e2Tag :: Tagged (a, b, c) () -> Tagged b ()
        e2Tag = retag

        e3Tag :: Tagged (a, b, c) () -> Tagged c ()
        e3Tag = retag

-}
