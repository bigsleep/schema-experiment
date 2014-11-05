{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts, DeriveGeneric #-}
module Main where

import Test.Hspec (hspec, Spec, describe, it, shouldBe, shouldSatisfy, shouldContain, expectationFailure)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

import Data.Aeson.Types (Options(..), defaultOptions, SumEncoding(..))
import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Instances
import Data.Aeson.Schema.Classes
import Data.Aeson.Schema.Generic

import qualified Data.Text as T
import Data.Tagged (Tagged(..))
import Data.Char (toLower)
import Data.Map.Strict (Map, empty, fromList)
import GHC.Generics (Generic)

-- string tag
data Data1 =
    Data1Con1 |
    Data1Con2 |
    Data1Con3 deriving (Show, Generic)
instance HasSchema Data1

-- string tag, modify con
data Data2 =
    Data2Con1 |
    Data2Con2 |
    Data2Con3 deriving (Show, Generic)
option2 = defaultOptions {constructorTagModifier = map toLower . drop 5}
instance HasSchema Data2 where
    schema = genericSchema option2

-- no string tag
data Data3 =
    Data3Con1 |
    Data3Con2 deriving (Show, Generic)
option3 = defaultOptions {allNullaryToStringTag = False, constructorTagModifier = map toLower . drop 5}
instance HasSchema Data3 where
    schema = genericSchema option3

-- single con, single type
data Data4 = Data4 [Int] deriving (Show, Generic)
instance HasSchema Data4

-- two cons, single type
data Data5 = Data5Con1 Bool | Data5Con2 Int deriving (Show, Generic)
instance HasSchema Data5

-- single con, two types
data Data6 = Data6Con1 Bool () deriving (Show, Generic)
instance HasSchema Data6

-- two cons, tow types
data Data7 = Data7Con1 Bool Int | Data7Con2 Int Bool deriving (Show, Generic)
instance HasSchema Data7

-- field labels
data Data8 = Data8Con1
    { _Data8Field1 :: Data1
    , _Data8Field2 :: String
    , _Data8Field3 :: [Int]
    } deriving (Show, Generic)
option8 = defaultOptions { fieldLabelModifier = map toLower . drop 6 }
instance HasSchema Data8 where
    schema = genericSchema option8

-- two cons, field labels
data Data9 = Data9Con1
    { _Data9Field1 :: Data1
    , _Data9Field2 :: Bool
    } |
    Data9Con2
    { _Data9Field3 :: Data5
    } deriving (Show, Generic)
option9 = defaultOptions { sumEncoding = ObjectWithSingleField, constructorTagModifier = drop 5, fieldLabelModifier = map toLower . drop 6 }
instance HasSchema Data9 where
    schema = genericSchema option9

-- type parameters
data Data10 a b c = Data10
    { _Data10Field1 :: a
    , _Data10Field2 :: [b]
    , _Data10Field3 :: Either b c
    } deriving (Show, Generic)
option10 = defaultOptions { fieldLabelModifier = map toLower . drop 7 }
instance (HasSchema a, HasSchema b, HasSchema c) => HasSchema (Data10 a b c) where
    schema = genericSchema option10

-- type parameter
data Data11 m a = Data11
    { _Data11Field1 :: m a
    , _Data11Field2 :: a
    , _Data11Field3 :: Int
    } deriving (Show, Generic)
option11 = defaultOptions { fieldLabelModifier = map toLower . drop 7 }
instance (HasSchema a, HasSchema (m a)) => HasSchema (Data11 m a) where
    schema = genericSchema option11

-- tree
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show, Generic)
instance (HasSchema a) => HasSchema (Tree a) where
    schema = genericSchema defaultOptions { sumEncoding = ObjectWithSingleField }

main :: IO ()
main = hspec $ do
    describe "Generic" $ do
        it "Data1, string tag" $ do
            let expected = (Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"])), empty)
            schema empty (tag :: Tag Data1) `shouldBe` expected

        it "Data2, string tag, modify cons" $ do
            let expected = (Schema (SchemaValueString (SchemaStringEnum ["con1", "con2", "con3"])), empty)
            schema empty (tag :: Tag Data2) `shouldBe` expected

        it "Data3, no string tag" $ do
            let expected =
                  ( SchemaAlias "Main.Data3"
                  , fromList
                    [
                      ( "Main.Data3"
                      , SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("con1", Schema . tuple $ [])
                        , ("con2", Schema . tuple $ [])
                        ]
                      )
                    ]
                  )
            schema empty (tag :: Tag Data3) `shouldBe` expected

        it "Data4, single con, single type" $ do
            let expected = (Schema . array . Schema $ number, empty)
            schema empty (tag :: Tag Data4) `shouldBe` expected

        it "Data5, twos con, single types" $ do
            let expected =
                  ( SchemaAlias "Main.Data5"
                  , fromList
                    [ ("Main.Data5",
                      SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("Data5Con1", Schema bool)
                        , ("Data5Con2", Schema number)]
                      )
                    ]
                  )
            schema empty (tag :: Tag Data5) `shouldBe` expected

        it "Data6, single con, two types" $ do
            let expected = (Schema . tuple $ [Schema $ bool, Schema . tuple $ []], empty)
            schema empty (tag :: Tag Data6) `shouldBe` expected

        it "Data7, two cons, two types" $ do
            let expected =
                  ( SchemaAlias "Main.Data7"
                  , fromList
                    [ ("Main.Data7"
                      , SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("Data7Con1", Schema . tuple $ [Schema bool, Schema number])
                        , ("Data7Con2", Schema . tuple $ [Schema number, Schema bool])
                        ]
                      )
                    ]
                  )
            schema empty (tag :: Tag Data7) `shouldBe` expected

        it "Data8, field labels" $ do
            let expected =
                  ( SchemaAlias "Main.Data8"
                  , fromList
                    [ ("Main.Data8"
                      , Schema . object $
                        [ ("field1", Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"])))
                        , ("field2", Schema string)
                        , ("field3", Schema . array . Schema $ number)
                        ]
                      )
                    ]
                  )
            schema empty (tag :: Tag Data8) `shouldBe` expected

        it "Data9, two cons, two field labels" $ do
            let expected =
                  ( SchemaAlias "Main.Data9"
                  , fromList
                    [ ("Main.Data9"
                      , SchemaUnion ObjectWithSingleField . fromList $
                        [ ("Con1", Schema . object $
                            [ ("field1", Schema (SchemaValueString (SchemaStringEnum ["Data1Con1", "Data1Con2", "Data1Con3"])))
                            , ("field2", Schema bool)
                            ]
                          )
                        , ("Con2",  Schema . object $
                            [ ("field3", SchemaAlias "Main.Data5")
                            ]
                          )
                        ]
                      )
                    , ("Main.Data5",
                        SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("Data5Con1", Schema bool)
                        , ("Data5Con2", Schema number)
                        ]
                      )
                    ]
                  )
            schema empty (tag :: Tag Data9) `shouldBe` expected

        it "Data10, type parameters" $ do
            let expected =
                  ( SchemaAlias "Main.Data10"
                  , fromList
                    [ ("Main.Data10"
                      , Schema . object $
                        [ ("field1", Schema number)
                        , ("field2", Schema . array . Schema $ bool)
                        , ("field3", SchemaAlias "Data.Either.Either")
                        ]
                      )
                    , ("Data.Either.Either"
                      , SchemaUnion (sumEncoding defaultOptions) . fromList $
                        [ ("Left", Schema bool)
                        , ("Right", Schema . array . Schema $ number)
                        ]
                      )
                    ]
                  )
            schema empty (tag :: Tag (Data10 Int Bool Data4)) `shouldBe` expected

        it "Data11, type parameters" $ do
            let expected =
                  ( SchemaAlias "Main.Data11"
                  , fromList
                    [ ("Main.Data11"
                    , Schema . object $
                      [ ("field1", Schema . array . Schema $ bool)
                      , ("field2", Schema bool)
                      , ("field3", Schema number)
                      ]
                      )
                    ]
                  )
            schema empty (tag :: Tag (Data11 [] Bool)) `shouldBe` expected

        it "Tree" $ do
            let expected =
                  ( SchemaAlias "Main.Tree"
                  , fromList
                    [ ("Main.Tree", SchemaUnion ObjectWithSingleField . fromList $
                      [ ("Node", Schema . tuple $ [Schema number, SchemaAlias "Main.Tree", SchemaAlias "Main.Tree"])
                      , ("Leaf", Schema . tuple $ [])
                      ]
                    ) ]
                  )
            schema empty (tag :: Tag (Tree Int)) `shouldBe` expected
