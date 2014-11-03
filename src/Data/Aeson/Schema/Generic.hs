{-# LANGUAGE CPP, DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances,
    ViewPatterns, NamedFieldPuns, FlexibleContexts, PatternGuards,
    RecordWildCards #-}
module Data.Aeson.Schema.Generic
( GHasSchema(..)
) where

import GHC.Generics
import Data.Aeson.Types (Options(..))
import Data.Aeson.Schema.Types
import Data.Text (Text, pack)
import qualified Data.Map.Strict as M (fromList)

---------------------------------------
class GHasSchema f where
    gschema :: Options -> Tag (f a) -> Schema

instance (GHasSchema a) => GHasSchema (M1 i c a) where
    gschema opts = gschema opts . retag
        where retag :: Tag (M1 i c a b) -> Tag (a b)
              retag = const tag

instance (ConsSchema a) => GHasSchema (D1 c a) where
    gschema opts tg = case consSchema opts (retag tg) of
                           [(_, s)] -> s
                           xs -> SchemaUnion sume . M.fromList $ xs
        where
        retag :: Tag (D1 c a p) -> Tag (a p)
        retag _ = tag
        sume = sumEncoding opts

instance (HasSchema a) => GHasSchema (K1 i a) where
    gschema _ _ = schema (tag :: Tag a)

instance GHasSchema U1 where
    gschema _ _ = Schema . tuple $ []

---------------------------------------
class ConsSchema f where
    consSchema :: Options -> Tag (f a) -> [(Text, Schema)]

class ConsSchema' f isRecord where
    consSchema' :: Tag isRecord -> Options -> Tag (f a) -> Schema

instance (ConsSchema a, ConsSchema b) => ConsSchema (a :+: b) where
    consSchema opts tg = consSchema opts (l tg) ++ consSchema opts (r tg)
        where
        l :: Tag ((a :+: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a :+: b) c) -> Tag (b c)
        r _ = tag

instance ( IsRecord a isRecord
         , ConsSchema' a isRecord
         , Constructor c) => ConsSchema (C1 c a) where
    consSchema opts tg = [(cname, consSchema' (tag :: Tag isRecord) opts (retag tg))]
        where
        retag :: Tag (C1 c a p) -> Tag (a p)
        retag _ = tag
        cname = pack . constructorTagModifier opts . getConName $ tg

instance (RecordSchema f) => ConsSchema' f True where
    consSchema' _ opts tg = Schema . object $ rschema opts tg

instance (NoSelSchema f) => ConsSchema' f False where
    consSchema' _ opts tg = case nsschema opts tg of
                                 [x] -> x
                                 xs -> Schema . tuple $ xs

------------------------------------------
class RecordSchema f where
    rschema :: Options -> Tag (f a) -> [(Text, Schema)]

instance (RecordSchema a, RecordSchema b) => RecordSchema (a :*: b) where
    rschema opts tg = rschema opts (l tg) ++ rschema opts (r tg)
        where
        l :: Tag ((a :*: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a :*: b) c) -> Tag (b c)
        r _ = tag

instance (Selector s, GHasSchema a) => RecordSchema (S1 s a) where
    rschema = fieldSchema

fieldSchema :: (Selector s, GHasSchema a) => Options -> Tag (S1 s a p) -> [(Text, Schema)]
fieldSchema opts tg = [(label, gschema opts (retag tg))]
    where
    retag :: Tag (S1 s a p) -> Tag (a p)
    retag _ = tag
    fieldMod = fieldLabelModifier opts
    label = pack . fieldMod . getSelName $ tg

------------------------------------------
class NoSelSchema f where
    nsschema :: Options -> Tag (f a) -> [Schema]

instance (NoSelSchema a, NoSelSchema b) => NoSelSchema (a :*: b) where
    nsschema opts tg = nsschema opts (l tg) ++ nsschema opts (r tg)
        where
        l :: Tag ((a :*: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a :*: b) c) -> Tag (b c)
        r _ = tag

instance (GHasSchema a) => NoSelSchema (S1 NoSelector a) where
    nsschema opts tg = [gschema opts (retag tg)]
        where
        retag :: Tag (S1 NoSelector a p) -> Tag (a p)
        retag _ = tag

--------------------------------------------------
data True
data False

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord U1 False

----------------------------------------------------
newtype M1T c (f :: * -> *) p = M1T ()

getSelName :: (Selector s) => Tag (S1 s a p) -> String
getSelName = selName . retag
    where
    retag :: Tag (S1 s a p) -> M1T s a p
    retag _ = M1T ()

getConName :: (Constructor c) => Tag (C1 c a p) -> String
getConName = conName . retag
    where
    retag :: Tag (C1 c a p) -> M1T c a p
    retag _ = M1T ()
