{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Data.Bson.Generic
( FromBSON(..)
, ToBSON(..)
) where

import           GHC.Generics
import qualified Data.Bson as BSON (lookup)
import           Data.Bson
import           Data.UString (u)
import           Data.Typeable
import           Control.Monad

------------------------------------------------------------------------------

instance (FromBSON a, ToBSON a, Typeable a, Show a, Eq a) => Val a where
    val         x = Doc $ toBSON x
    cast' (Doc x) = fromBSON x
    cast' _       = Nothing

------------------------------------------------------------------------------

class ToBSON a where
    toBSON :: a -> Document

    default toBSON :: (Generic a, GenericToBSON (Rep a)) => a -> Document
    toBSON a = genericToBSON (from a)

class GenericToBSON f where
    genericToBSON :: f a -> Document

-- | Unit type -> Empty document
instance GenericToBSON U1 where
    genericToBSON U1 = []

-- | Sum of types
instance (GenericToBSON a, GenericToBSON b) => GenericToBSON (a :*: b) where
    genericToBSON (x :*: y) = genericToBSON x ++ genericToBSON y

-- | Product of types
instance (GenericToBSON a, GenericToBSON b) => GenericToBSON (a :+: b) where
    genericToBSON (L1 x) = genericToBSON x
    genericToBSON (R1 x) = genericToBSON x

-- | Datatype information tag
instance (GenericToBSON a) => GenericToBSON (D1 c a) where
    genericToBSON (M1 x) = genericToBSON x

-- | Constructor tag
instance (GenericToBSON a, Constructor c) => GenericToBSON (M1 C c a) where
    genericToBSON c@(M1 x) = genericToBSON x ++ [ u "_constructor" =: u (conName c)]

-- | Selector tag
instance (Val a, Selector s) => GenericToBSON (M1 S s (K1 i a)) where
    genericToBSON s@(M1 (K1 x)) = [u (selName s) =: x]

-- | Constants
instance (ToBSON a) => GenericToBSON (K1 i a) where
    genericToBSON (K1 x) = toBSON x

------------------------------------------------------------------------------

------------------------------------------------------------------------------

class FromBSON a where
    fromBSON :: Document -> Maybe a

    default fromBSON :: (Generic a, GenericFromBSON (Rep a)) => Document -> Maybe a
    fromBSON doc = maybe Nothing (Just . to) (genericFromBSON doc)

class GenericFromBSON f where
    genericFromBSON :: Document -> Maybe (f a)

instance GenericFromBSON U1 where
    genericFromBSON doc = Just U1

instance (GenericFromBSON a, GenericFromBSON b) => GenericFromBSON (a :*: b) where
    genericFromBSON doc = do
        x <- (genericFromBSON doc)
        y <- (genericFromBSON doc)
        return (x :*: y)

instance (GenericFromBSON a, GenericFromBSON b) => GenericFromBSON (a :+: b) where
    genericFromBSON doc = left `mplus` right
        where left  = maybe Nothing (Just . L1) (genericFromBSON doc)
              right = maybe Nothing (Just . R1) (genericFromBSON doc)

instance (GenericFromBSON a, Constructor c) => GenericFromBSON (M1 C c a) where
    genericFromBSON doc = do
        cname <- BSON.lookup (u "_constructor") doc
        if (cname == (conName (undefined :: M1 C c a r)))
           then maybe Nothing (Just . M1) (genericFromBSON doc)
           else Nothing

instance (GenericFromBSON a) => GenericFromBSON (M1 D c a) where
    genericFromBSON doc = genericFromBSON doc >>= return . M1

instance (Val a, Selector s) => GenericFromBSON (M1 S s (K1 i a)) where
    genericFromBSON doc = (BSON.lookup sname doc) >>= return . M1 . K1
        where sname = u . selName $ (undefined :: M1 S s (K1 i a) r)

------------------------------------------------------------------------------

{-
data Test0 = A | B | C deriving (Generic, Typeable, Show, Eq)
instance ToBSON Test0
instance FromBSON Test0
-- (fromBSON $ toBSON A) :: Maybe Test0

data Test1 = Test1 String String deriving (Generic, Typeable, Show, Eq)
instance ToBSON Test1
instance FromBSON Test1
-- (fromBSON $ toBSON $ Test1 "aa" "bb") :: Maybe Test1

data Test2 = Test2 { test20 :: String, test21 :: String } deriving (Generic, Typeable, Show, Eq)
instance ToBSON Test2
instance FromBSON Test2
-- (fromBSON $ toBSON $ Test2 "aa" "bb") :: Maybe Test2

data Test3 = Test3 { test30 :: Test2, test31 :: String } deriving (Generic, Typeable, Show, Eq)
instance ToBSON Test3
instance FromBSON Test3
-- (fromBSON $ toBSON $ Test3 (Test2 "aa" "bb") "cc") :: Maybe Test3
-}