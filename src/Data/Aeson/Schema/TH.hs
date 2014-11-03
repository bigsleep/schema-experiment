{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Data.Aeson.Schema.TH
( deriveHasSchema
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..), lift)
import qualified Data.Aeson.TH as DA
import Data.Aeson.Schema.Types
import qualified Data.Text as T (Text, pack, unpack)
import Data.Map.Strict (fromList)

deriveHasSchema :: DA.Options -> Name -> DecsQ
deriveHasSchema options name = do
    (tvbs, cons) <- typeInfo name
    tag <- newName "tag"
    let tvbNames = map tvbName tvbs
        cxts = mapM (classP ''HasSchema . (:[]) . varT) tvbNames
        tq = foldl appT (conT name) (map varT tvbNames)
        ty = conT ''HasSchema `appT` tq

    t <- tq
    (expr, decs) <- genFromCons options t tvbs tag cons
    let body = normalB (return expr)
        decqs = [funD 'schema [clause [varP tag] body (map return decs)]]
    fmap (:[]) $ instanceD cxts ty decqs

genFromCons :: DA.Options -> Type -> [TyVarBndr] -> Name -> [Con] -> Q (Exp, [Dec])
genFromCons _ _ _ _ [] = error "no construcotrs"
genFromCons options t tvbs tag [con] =
    genSchemaCon options t tvbs tag con >>= return . snd
genFromCons options t tvbs tag cons
    | useStringTag = genStringEnum . lift $ modConNames
    | otherwise = do
        xs <- mapM (genSchemaCon options t tvbs tag) cons
        let ys = zip (map fst xs) (map (fst . snd) xs)
        expr <- encodeSum options ys
        let decs = concat . map (snd . snd) $ xs
        return (expr, decs)
    where
    useStringTag = DA.allNullaryToStringTag options && all isNullary cons
    conMod = DA.constructorTagModifier options
    conNames = map getConName cons
    modConNames = map (T.pack . conMod . dropNameSpace . show) conNames
    genStringEnum cns = do
        expr <- [|Schema . enum $ ($cns)|]
        return (expr, [])

genSchemaCon :: DA.Options -> Type -> [TyVarBndr] -> Name -> Con -> Q (Name, (Exp, [Dec]))

genSchemaCon _ t tvbs tag (NormalC name [ty]) = do
    (retagName, retagDecs) <- genRetag tvbs t . snd $ ty
    expr <- (varE 'schema `appE`) . (`appE` varE tag) . varE $ retagName
    return (name, (expr, retagDecs))

genSchemaCon _ t tvbs tag (NormalC name types) = do
    (retagNames, retagDecs) <- fmap unzip $ mapM (genRetag tvbs t . snd) types
    let schemas = listE $ map ((varE 'schema `appE`) . (`appE` varE tag) . varE) retagNames
    expr <- [|Schema . tuple $ ($schemas)|]
    return (name, (expr, concat retagDecs))

genSchemaCon options t tvbs tag (RecC name fields) = do
    (retagNames, retagDecs) <- fmap unzip $ mapM (genRetag tvbs t . getFieldType) fields
    let fieldNames = map (T.pack . fieldMod . dropNameSpace . show . getFieldName) fields
        schemas = map ((varE 'schema `appE`) . (`appE` varE tag) . varE) retagNames
        fieldsExpr = listE . map tupq $ zip (map lift fieldNames) schemas
    expr <- [|Schema . object $ ($fieldsExpr)|]
    return (name, (expr, concat retagDecs))
    where
    getFieldName (a, _, _) = a
    getFieldType (_, _, a) = a
    fieldMod = DA.fieldLabelModifier options
    tupq (a, b) = tupE [a, b]

genSchemaCon _ _ _ _ _ = error "error"


genRetag :: [TyVarBndr] -> Type -> Type -> Q (Name, [Dec])
genRetag tvbs arg ret = do
    fname <- newName "retagf"
    let targ = return arg
        tret = return ret
        ty = [t|Tag ($targ) -> Tag ($tret)|]
    dec <- funD fname [clause [wildP] (normalB (varE 'tag)) []]
    sig <- sigD fname $ forallT tvbs (return []) ty
    return (fname, [sig, dec])

encodeSum :: DA.Options -> [(Name, Exp)] -> Q Exp
encodeSum options infos =
    genSchemaSum sumQ . listE $ map pairQ infos
    where
    sumQ = lift . DA.sumEncoding $ options
    pairQ (name, expr) = [|($conTag, $(return expr))|]
        where
        conName = dropNameSpace . show $ name
        conTag = lift . T.pack . DA.constructorTagModifier options $ conName

genSchemaSum :: ExpQ -> ExpQ ->  ExpQ
genSchemaSum sumQ fields = [|SchemaUnion ($sumQ) . fromList $ ($fields)|]

tvbName :: TyVarBndr -> Name
tvbName (PlainTV  name  ) = name
tvbName (KindedTV name _) = name

getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

typeInfo :: Name -> Q ([TyVarBndr], [Con])
typeInfo = (f =<<) . reify
    where
    f (TyConI (DataD _ _ tvbs cons _)) = return (tvbs, cons)
    f (TyConI (NewtypeD _ _ tvbs con _)) = return (tvbs, [con])
    f _ = error $ "type name required."

isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

dropNameSpace :: String -> String
dropNameSpace = reverse . takeWhile (/= '.') . reverse

instance Lift T.Text where
    lift t = litE (stringL (T.unpack t))

instance Lift DA.SumEncoding where
    lift DA.ObjectWithSingleField = [|DA.ObjectWithSingleField|]
    lift DA.TwoElemArray = [|DA.TwoElemArray|]
    lift (DA.TaggedObject a b) = [|(DA.TaggedObject a b)|]
