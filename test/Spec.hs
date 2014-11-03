{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldSatisfy, shouldContain, expectationFailure)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

import Data.Aeson.TH (Options(..), defaultOptions, SumEncoding(..))
import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Instances
import Data.Aeson.Schema.TH (deriveHasSchema)

import qualified Data.Text as T
import Data.Tagged (Tagged(..))
import Data.Char (toLower)
import Data.Map.Strict (fromList)

-- string tag
data Data1 =
    Data1Con1 |
    Data1Con2 |
    Data1Con3
deriveHasSchema defaultOptions ''Data1

-- string tag, modify con
data Data2 =
    Data2Con1 |
    Data2Con2 |
    Data2Con3
deriveHasSchema defaultOptions {constructorTagModifier = map toLower . drop 5} ''Data2

-- no string tag
data Data3 =
    Data3Con1 |
    Data3Con2
deriveHasSchema defaultOptions {allNullaryToStringTag = False, constructorTagModifier = map toLower . drop 5} ''Data3

-- single con, single type
data Data4 = Data4 [Int]
deriveHasSchema defaultOptions ''Data4

-- two cons, single type
data Data5 = Data5Con1 Bool | Data5Con2 Int
deriveHasSchema defaultOptions ''Data5

-- single con, two types
data Data6 = Data6Con1 Bool ()
deriveHasSchema defaultOptions ''Data6

-- two cons, tow types
data Data7 = Data7Con1 Bool Int | Data7Con2 Int Bool
deriveHasSchema defaultOptions ''Data7

-- field labels
data Data8 = Data8Con1
    { _Data8Field1 :: Data1
    , _Data8Field2 :: String
    , _Data8Field3 :: [Int]
    }
deriveHasSchema defaultOptions { fieldLabelModifier = map toLower . drop 6 } ''Data8

-- two cons, field labels
data Data9 = Data9Con1
    { _Data9Field1 :: Data1
    , _Data9Field2 :: Bool
    } |
    Data9Con2
    { _Data9Field3 :: Data4
    }
deriveHasSchema defaultOptions { sumEncoding = ObjectWithSingleField, constructorTagModifier = drop 5, fieldLabelModifier = map toLower . drop 6 } ''Data9

-- type parameters
data Data10 a b c = Data10
    { _Data10Field1 :: a
    , _Data10Field2 :: [b]
    , _Data10Field3 :: Either b c
    }
deriveHasSchema defaultOptions { fieldLabelModifier = map toLower . drop 7 } ''Data10

-- type parameter
data Data11 m a = Data11
    { _Data11Field1 :: m a
    , _Data11Field2 :: a
    , _Data11Field3 :: Int
    }
{-
deriveHasSchema defaultOptions { fieldLabelModifier = map toLower . drop 7 } ''Data11
-}
instance (HasSchema a, HasSchema (m a)) => HasSchema (Data11 m a) where
    schema _ = Schema bool

main :: IO ()
main = hspec $ do
    describe "deriveHasSchema" $ do

        it "Data1, string tag" $ do
            let expected = Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"]))
            schema (tag :: Tag Data1) `shouldBe` expected

        it "Data2, string tag, modify cons" $ do
            let expected = Schema (SchemaValueString (SchemaStringEnum ["con1", "con2", "con3"]))
            schema (tag :: Tag Data2) `shouldBe` expected

        it "Data3, no string tag" $ do
            let expected = SchemaUnion (sumEncoding defaultOptions) . fromList $
                    [ ("con1", Schema . tuple $ [])
                    , ("con2", Schema . tuple $ [])]
            schema (tag :: Tag Data3) `shouldBe` expected

        it "Data4, single con, single type" $ do
            let expected = Schema . array . Schema $ number
            schema (tag :: Tag Data4) `shouldBe` expected

        it "Data5, twos con, single types" $ do
            let expected = SchemaUnion (sumEncoding defaultOptions) . fromList $
                    [ ("Data5Con1", Schema bool)
                    , ("Data5Con2", Schema number)]
            schema (tag :: Tag Data5) `shouldBe` expected

        it "Data6, single con, two types" $ do
            let expected = Schema . tuple $ [Schema $ bool, Schema . tuple $ []]
            schema (tag :: Tag Data6) `shouldBe` expected

        it "Data7, two cons, two types" $ do
            let expected = SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("Data7Con1", Schema . tuple $ [Schema bool, Schema number])
                        , ("Data7Con2", Schema . tuple $ [Schema number, Schema bool])]
            schema (tag :: Tag Data7) `shouldBe` expected

        it "Data8, field labels" $ do
            let expected = Schema . object $
                    [ ("field1", Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"])))
                    , ("field2", Schema string)
                    , ("field3", Schema . array . Schema $ number)
                    ]
            schema (tag :: Tag Data8) `shouldBe` expected

        it "Data9, two cons, two field labels" $ do
            let expected = SchemaUnion ObjectWithSingleField . fromList $
                  [ ("Con1", Schema . object $
                      [ ("field1", Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"])))
                      , ("field2", Schema bool)
                      ]
                    )
                  , ("Con2", Schema . object $
                      [ ("field3", Schema . array . Schema $ number) ]
                    )]
            schema (tag :: Tag Data9) `shouldBe` expected

        it "Data10, type parameters" $ do
            let expected = Schema . object $
                  [ ("field1", Schema number)
                  , ("field2", Schema . array . Schema $ bool)
                  , ("field3", SchemaUnion ObjectWithSingleField . fromList $
                    [ ("Left", Schema bool)
                    , ("Right", Schema . array . Schema $ number)
                    ])
                  ]
            schema (tag :: Tag (Data10 Int Bool Data4)) `shouldBe` expected


