{-# LANGUAGE OverloadedStrings, FlexibleInstances, OverlappingInstances, ScopedTypeVariables #-}
module Data.Aeson.Schema.Instances
(
) where

import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Classes (HasSchema(..))
import Data.Aeson.Schema.Generic ()
import Data.Aeson.Types (Options(..), SumEncoding(..), defaultOptions)

import Control.Monad (mapM, liftM2)
import Data.Scientific (Scientific)
import Data.Fixed (HasResolution, Fixed)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as T (Text, pack)
import Data.Ratio (Ratio)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M (Map, empty, insert, union)
import Data.Typeable (Typeable)

instance (HasSchema a) => HasSchema (Maybe a) where
    schema m t = (SchemaMaybe s, d)
        where
        (s, d) = schema m . etag $ t

instance HasSchema Bool where
    schema m _ = (Schema bool, m)

instance HasSchema [Char] where
    schema m _ = (Schema string, m)

instance HasSchema Char where
    schema m _ = (Schema string, m)

instance HasSchema Scientific where
    schema m _ = (Schema number, m)

instance HasSchema Double where
    schema m _ = (Schema number, m)

instance HasSchema Float where
    schema m _ = (Schema number, m)

instance HasSchema (Ratio Integer) where
    schema m _ = (s, m)
        where
        s = Schema . object $
            [ ("numerator", Schema number)
            , ("denominator", Schema number)
            ]

instance (HasResolution a) => HasSchema (Fixed a) where
    schema m _ = (Schema number, m)

instance HasSchema Int where
    schema m _ = (Schema number, m)

instance HasSchema Integer where
    schema m _ = (Schema number, m)

instance HasSchema Int8 where
    schema m _ = (Schema number, m)

instance HasSchema Int16 where
    schema m _ = (Schema number, m)

instance HasSchema Int32 where
    schema m _ = (Schema number, m)

instance HasSchema Int64 where
    schema m _ = (Schema number, m)

instance HasSchema Word where
    schema m _ = (Schema number, m)

instance HasSchema Word8 where
    schema m _ = (Schema number, m)

instance HasSchema Word16 where
    schema m _ = (Schema number, m)

instance HasSchema Word32 where
    schema m _ = (Schema number, m)

instance HasSchema Word64 where
    schema m _ = (Schema number, m)

instance HasSchema T.Text where
    schema m _ = (Schema string, m)

instance HasSchema LT.Text where
    schema m _ = (Schema string, m)

instance HasSchema a => HasSchema [a] where
    schema m t = (Schema . array $ s, d)
        where
        (s, d) = schema m . etag $ t

instance HasSchema a => HasSchema (Set.Set a) where
    schema m t = (Schema . array $ s, d)
        where
        (s, d) = schema m . etag $ t

instance HasSchema a => HasSchema (HashSet.HashSet a) where
    schema m t = (Schema . array $ s, d)
        where
        (s, d) = schema m . etag $ t

instance HasSchema IntSet.IntSet where
    schema m _ = (Schema . array . Schema $ number, m)

instance (HasSchema a) => HasSchema (IntMap.IntMap a) where
    schema m t = (s', d)
        where
        s' = Schema . array . Schema . tuple $ [Schema number, s]
        (s, d) = schema m . etag $ t

etag :: Tag (m a) -> Tag a
etag _ = tag


{-
import Data.List (intersperse, intercalate)

main :: IO ()
main = mapM_ (putStrLn . gen) $ 0 : [2..10]
    where
    as = flip take ['a'..]
    ty = intersperse ',' . as
    cxts = intercalate "," . map (("HasSchema " ++) . (:[])) . as
    decInstance a b = "instance (" ++ a ++ ") => HasSchema (" ++ b ++ ")"
    gen l = decInstance (cxts l) (ty l)
-}

instance () => HasSchema ()
instance (HasSchema a,HasSchema b) => HasSchema (a,b)
instance (HasSchema a,HasSchema b,HasSchema c) => HasSchema (a,b,c)
instance (HasSchema a,HasSchema b,HasSchema c,HasSchema d) => HasSchema (a,b,c,d)
instance (HasSchema a,HasSchema b,HasSchema c,HasSchema d,HasSchema e) => HasSchema (a,b,c,d,e)
instance (HasSchema a,HasSchema b,HasSchema c,HasSchema d,HasSchema e,HasSchema f) => HasSchema (a,b,c,d,e,f)
instance (HasSchema a,HasSchema b,HasSchema c,HasSchema d,HasSchema e,HasSchema f,HasSchema g) => HasSchema (a,b,c,d,e,f,g)

instance (HasSchema a, HasSchema b) => HasSchema (Either a b)
