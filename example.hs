{-# LANGUAGE DeriveGeneric #-}
module Main where
import Data.Aeson.Types (Options(..), defaultOptions, SumEncoding(..))
import Data.Aeson.Schema (schema, HasSchema(..), tag, Tag, genericSchema)
import Data.Aeson (encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as L

data FLAGS = AAA | BBB | CCC  deriving (Show, Eq, Generic)

instance HasSchema FLAGS

data Hoge = Hoge
    { hogeTree :: Tree Int
    , hogeArray :: [String]
    , hogeTuple :: (Int, Bool, String)
    , hogeFlag :: FLAGS
    } deriving (Show, Eq, Generic)

instance HasSchema Hoge where
    schemaJSON = genericSchema opts
        where
        opts = defaultOptions { fieldLabelModifier = drop 4 }

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show, Eq, Generic)

instance (HasSchema a) => HasSchema (Tree a) where
    schemaJSON = genericSchema defaultOptions { sumEncoding = ObjectWithSingleField }

main :: IO ()
main = do
    let s = schema (tag :: Tag Hoge)
    L.putStrLn . encode $ s
