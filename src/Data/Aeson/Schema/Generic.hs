{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Aeson.Schema.Generic
(
) where

import qualified GHC.Generics as G (M1, D1, C1, K1, U1, S1, S, (:+:), (:*:), Datatype(..), Constructor(..), Selector(..), NoSelector)
import Data.Aeson.Types (Options(..))
import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Classes
import qualified Data.Text as T (Text, pack)
import qualified Data.Map.Strict as M (Map, fromList, insert, lookup, delete)
import Control.Monad (liftM2)
import Control.Monad.State.Strict (State, modify, get, put)

--------------------------------------------------------------------------------
instance (GHasSchema a) => GHasSchema (G.M1 i c a) where
    gschema opts = gschema opts . retag
        where retag :: Tag (G.M1 i c a b) -> Tag (a b)
              retag = const tag

instance (ConsSchema a, G.Datatype c) => GHasSchema (G.D1 c a) where
    gschema opts tg = do
        x <- fmap (M.lookup typeName) get
        case x of
            Just _ -> return $ SchemaAlias typeName
            Nothing -> do
                modify (M.insert typeName (SchemaAlias typeName))
                putResult . apply =<< consSchema opts (retag tg)
        where
        typeName = T.pack . getDatatypeName $ (tag :: Tag (G.D1 c a ()))
        useStringTag = allNullaryToStringTag opts
        retag :: Tag (G.D1 c a p) -> Tag (a p)
        retag _ = tag
        sumE = sumEncoding opts

        apply [(_, x)] = x
        apply xs | useStringTag && all isNullary xs = Schema . enum . map fst $ xs
        apply xs = SchemaUnion sumE . M.fromList $ xs

        putResult s | isObject s || isUnion s = modify (M.insert typeName s) >> return (SchemaAlias typeName)
        putResult s = modify (M.delete typeName) >> return s

        isNullary (_, Schema (SchemaValueArray (SchemaArrayTuple []))) = True
        isNullary _ = False

instance (HasSchema a) => GHasSchema (G.K1 i a) where
    gschema _ _ = do
        m <- get
        let (s, m') = schemaJSON m (tag :: Tag a)
        put m'
        return s

instance GHasSchema G.U1 where
    gschema _ _ = return . Schema . tuple $ []

--------------------------------------------------------------------------------
class ConsSchema f where
    consSchema :: Options -> Tag (f a) -> State (M.Map T.Text Schema) [(T.Text, Schema)]

class ConsSchema' f isRecord where
    consSchema' :: Tag isRecord -> Options -> Tag (f a) -> State (M.Map T.Text Schema) Schema

instance (ConsSchema a, ConsSchema b) => ConsSchema (a G.:+: b) where
    consSchema opts tg = consSchema opts (l tg) `concatM` consSchema opts (r tg)
        where
        l :: Tag ((a G.:+: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a G.:+: b) c) -> Tag (b c)
        r _ = tag

instance ( IsRecord a isRecord
         , ConsSchema' a isRecord
         , G.Constructor c) => ConsSchema (G.C1 c a) where
    consSchema opts tg = do
        x <- consSchema' (tag :: Tag isRecord) opts (retag tg)
        return [(cname, x)]
        where
        retag :: Tag (G.C1 c a p) -> Tag (a p)
        retag _ = tag
        cname = T.pack . constructorTagModifier opts . getConName $ tg

instance (RecordSchema f) => ConsSchema' f True where
    consSchema' _ opts tg = return . Schema . object =<< rschema opts tg

instance (NoSelSchema f) => ConsSchema' f False where
    consSchema' _ opts tg = return . apply =<< nsschema opts tg
        where
        apply [x] = x
        apply xs =  Schema . tuple $ xs

--------------------------------------------------------------------------------
class RecordSchema f where
    rschema :: Options -> Tag (f a) -> State (M.Map T.Text Schema) [(T.Text, Schema)]

instance (RecordSchema a, RecordSchema b) => RecordSchema (a G.:*: b) where
    rschema opts tg = rschema opts (l tg) `concatM` rschema opts (r tg)
        where
        l :: Tag ((a G.:*: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a G.:*: b) c) -> Tag (b c)
        r _ = tag

instance (G.Selector s, GHasSchema a) => RecordSchema (G.S1 s a) where
    rschema = fieldSchema

fieldSchema :: (G.Selector s, GHasSchema a) => Options -> Tag (G.S1 s a p) -> State (M.Map T.Text Schema) [(T.Text, Schema)]
fieldSchema opts tg = do
    x <- gschema opts (retag tg)
    return [(label, x)]
    where
    retag :: Tag (G.S1 s a p) -> Tag (a p)
    retag _ = tag
    fieldMod = fieldLabelModifier opts
    label = T.pack . fieldMod . getSelName $ tg

--------------------------------------------------------------------------------
class NoSelSchema f where
    nsschema :: Options -> Tag (f a) -> State (M.Map T.Text Schema) [Schema]

instance (NoSelSchema a, NoSelSchema b) => NoSelSchema (a G.:*: b) where
    nsschema opts tg = nsschema opts (l tg) `concatM` nsschema opts (r tg)
        where
        l :: Tag ((a G.:*: b) c) -> Tag (a c)
        l _ = tag
        r :: Tag ((a G.:*: b) c) -> Tag (b c)
        r _ = tag

instance (GHasSchema a) => NoSelSchema (G.S1 G.NoSelector a) where
    nsschema opts tg = fmap (:[]) $ gschema opts (retag tg)
        where
        retag :: Tag (G.S1 G.NoSelector a p) -> Tag (a p)
        retag _ = tag

instance NoSelSchema G.U1 where
    nsschema _ _ = return [Schema . tuple $ []]

--------------------------------------------------------------------------------
data True
data False

class IsRecord (f :: * -> *) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f G.:*: g) isRecord
instance IsRecord (G.M1 G.S G.NoSelector f) False
instance (IsRecord f isRecord) => IsRecord (G.M1 G.S c f) isRecord
instance IsRecord (G.K1 i c) True
instance IsRecord G.U1 False

--------------------------------------------------------------------------------
newtype M1T c (f :: * -> *) p = M1T ()

getSelName :: (G.Selector s) => Tag (G.S1 s a p) -> String
getSelName = G.selName . retag
    where
    retag :: Tag (G.S1 s a p) -> M1T s a p
    retag _ = M1T ()

getConName :: (G.Constructor c) => Tag (G.C1 c a p) -> String
getConName = G.conName . retag
    where
    retag :: Tag (G.C1 c a p) -> M1T c a p
    retag _ = M1T ()

getDatatypeName ::  (G.Datatype d) => Tag (G.D1 d a p) -> String
getDatatypeName t = G.moduleName t' ++ '.' : G.datatypeName t'
    where
    retag :: Tag (G.D1 d a p) -> M1T d a p
    retag _ = M1T ()
    t' = retag t

concatM :: (Monad m) => m [a] -> m [a] -> m [a]
concatM = liftM2 (++)
