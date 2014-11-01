{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.Aeson.Schema.TH
( tupleInstanceOfHasSchema
) where

import Control.Monad (replicateM, liftM2)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (Lift(..))
import Data.Tagged (Tagged, retag)
import Data.Aeson.Schema.Types

tupleInstanceOfHasSchema :: Int -> DecsQ
tupleInstanceOfHasSchema n = do
    es <- replicateM n $ newName "e"
    tag <- newName "tag"

    let cxts = mapM (classP ''HasSchema . (:[]) . varT) es
        ty = conT ''HasSchema `appT` foldl appT (tupleT n) (map varT es)

    retags <- mapM (genRetag n) [1..n]
    let retagNames = map fst retags
        retagDecs = concat . map snd $ retags
        schemas = listE $ map ((varE 'schema `appE`) . (`appE` varE tag) . varE) retagNames
        f = genSchemaDec tag schemas retagDecs
    fmap return $ instanceD cxts ty [f]

genRetag :: Int -> Int -> Q (Name, [Dec])
genRetag tsize i | tsize < i || i < 1 = error "bad arg"
                          | otherwise = do
    es <- replicateM tsize $ newName "e"
    fname <- newName "retagf"
    let tup = foldl appT (tupleT tsize) (map varT es)
        ret = varT $ es !! (i - 1)
        ty = [t|Tagged ($tup) () -> Tagged ($ret) ()|]
    dec <- funD fname [clause [] (normalB (varE 'retag)) []]
    sig <- sigD fname $ forallT (map plainTV es) (return []) ty
    return (fname, [sig, dec])

genSchemaDec :: Name -> ExpQ -> [Dec] -> DecQ
genSchemaDec tag schemas decs = do
        expr <- [e|Schema . tuple $ ($schemas)|]
        return $ FunD 'schema [Clause [VarP tag] (NormalB expr) decs]
