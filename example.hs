{-# LANGUAGE DeriveGeneric #-}
module Main where
import Data.Aeson.Types (Options(..), defaultOptions, SumEncoding(..))
import Data.Aeson.Schema.Types
import Data.Aeson.Schema.Instances
import Data.Aeson.Schema.Classes
import Data.Aeson.Schema.Generic
import Data.Aeson (encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M (empty)

data FLAGS = AAA | BBB | CCC  deriving (Show, Eq, Generic)

instance HasSchema FLAGS

data Hoge = Hoge
    { hogeTree :: Tree Int
    , hogeArray :: [String]
    , hogeTuple :: (Int, Bool, String)
    , hogeFlag :: FLAGS
    } deriving (Show, Eq, Generic)

instance HasSchema Hoge where
    schema = genericSchema opts
        where
        opts = defaultOptions { fieldLabelModifier = drop 4 }

data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show, Eq, Generic)

instance (HasSchema a) => HasSchema (Tree a) where
    schema = genericSchema defaultOptions { sumEncoding = ObjectWithSingleField }

main :: IO ()
main = do
    let s = schema M.empty (tag :: Tag Hoge)
    L.putStrLn . encode $ s
