{-# LANGUAGE OverloadedStrings, OverlappingInstances, FlexibleInstances #-}
module Data.Aeson.Schema.Instances
(
) where

import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Classes (HasSchema(..))
import Data.Aeson.Schema.Generic ()

import Data.Scientific (Scientific)
import Data.Fixed (HasResolution, Fixed)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Text as T (Text)
import Data.Ratio (Ratio)
import qualified Data.Text.Lazy as LT (Text)
import qualified Data.Set as Set (Set)
import qualified Data.HashSet as HashSet (HashSet)
import qualified Data.IntMap as IntMap (IntMap)
import qualified Data.IntSet as IntSet (IntSet)

instance (HasSchema a) => HasSchema (Maybe a) where
    schemaJSON m t = (SchemaMaybe s, d)
        where
        (s, d) = schemaJSON m . etag $ t

instance HasSchema Bool where
    schemaJSON m _ = (Schema bool, m)

instance HasSchema [Char] where
    schemaJSON m _ = (Schema string, m)

instance HasSchema Char where
    schemaJSON m _ = (Schema string, m)

instance HasSchema Scientific where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Double where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Float where
    schemaJSON m _ = (Schema number, m)

instance HasSchema (Ratio Integer) where
    schemaJSON m _ = (s, m)
        where
        s = Schema . object $
            [ ("numerator", Schema number)
            , ("denominator", Schema number)
            ]

instance (HasResolution a) => HasSchema (Fixed a) where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Int where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Integer where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Int8 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Int16 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Int32 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Int64 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Word where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Word8 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Word16 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Word32 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema Word64 where
    schemaJSON m _ = (Schema number, m)

instance HasSchema T.Text where
    schemaJSON m _ = (Schema string, m)

instance HasSchema LT.Text where
    schemaJSON m _ = (Schema string, m)

instance HasSchema a => HasSchema [a] where
    schemaJSON m t = (Schema . array $ s, d)
        where
        (s, d) = schemaJSON m . etag $ t

instance HasSchema a => HasSchema (Set.Set a) where
    schemaJSON m t = (Schema . array $ s, d)
        where
        (s, d) = schemaJSON m . etag $ t

instance HasSchema a => HasSchema (HashSet.HashSet a) where
    schemaJSON m t = (Schema . array $ s, d)
        where
        (s, d) = schemaJSON m . etag $ t

instance HasSchema IntSet.IntSet where
    schemaJSON m _ = (Schema . array . Schema $ number, m)

instance (HasSchema a) => HasSchema (IntMap.IntMap a) where
    schemaJSON m t = (s', d)
        where
        s' = Schema . array . Schema . tuple $ [Schema number, s]
        (s, d) = schemaJSON m . etag $ t

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
