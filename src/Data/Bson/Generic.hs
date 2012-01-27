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

------------------------------------------------------------------------------
-- | Examples
--
-- > data Test0 = A | B | C deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test0
-- > instance FromBSON Test0
-- >
-- > (fromBSON $ toBSON A) :: Maybe Test0
--
--
-- > data Test1 = Test1 String String deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test1
-- > instance FromBSON Test1
-- >
-- > (fromBSON $ toBSON $ Test1 "aa" "bb") :: Maybe Test1
--
--
-- > data Test2 = Test2 { test20 :: String, test21 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test2
-- > instance FromBSON Test2
-- >
-- > (fromBSON $ toBSON $ Test2 "aa" "bb") :: Maybe Test2
--
--
-- > data Test3 = Test3 { test30 :: Test2, test31 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test3
-- > instance FromBSON Test3
-- >
-- > (fromBSON $ toBSON $ Test3 (Test2 "aa" "bb") "cc") :: Maybe Test3
--
--
-- > data Test4 = Test4 { test4Key :: ObjectKey, test4 :: String } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Test4
-- > instance FromBSON Test4
-- >
-- > (fromBSON $ toBSON $ Test4 (unsafePerformIO genObjectId) "something") :: Maybe Test4
-- > (fromBSON $ toBSON $ Test4 Nothing "something") :: Maybe Test4
--
--
-- > data Comment = Comment { author :: String, comments :: [Comment] } deriving (Generic, Typeable, Show, Eq)
-- > instance ToBSON Comment
-- > instance FromBSON Comment
-- >
-- > (fromBSON $ toBSON $ Comment "Joe1" [Comment "Joe2" [], Comment "Joe3" [Comment "Joe4" []]]) :: Maybe Comment

module Data.Bson.Generic
( ToBSON(..)
, FromBSON(..)
, ObjectKey(..)
, keyLabel
) where

import           GHC.Generics
import qualified Data.Bson as BSON (lookup)
import           Data.Bson
import           Data.UString (u)
import           Data.Typeable
import           Control.Monad

keyLabel :: Label
keyLabel = u "_id"

------------------------------------------------------------------------------

newtype ObjectKey = ObjectKey { unObjectKey :: Maybe ObjectId } deriving (Generic, Typeable, Show, Eq)
instance FromBSON ObjectKey
instance ToBSON ObjectKey

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
instance (GenericToBSON a, Constructor c) => GenericToBSON (C1 c a) where
    genericToBSON c@(M1 x) = genericToBSON x

-- | Selector tag
instance (Val a, Selector s) => GenericToBSON (S1 s (K1 i a)) where
    genericToBSON s@(M1 (K1 x)) = [u (selName s) =: x]

-- | ObjectKey special treatment
instance (Selector s) => GenericToBSON (S1 s (K1 i ObjectKey)) where
    genericToBSON (M1 (K1 (ObjectKey (Just key)))) = [ keyLabel =: key ]
    genericToBSON                                _ = []

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

instance (GenericFromBSON a, Constructor c) => GenericFromBSON (C1 c a) where
    genericFromBSON doc = maybe Nothing (Just . M1) (genericFromBSON doc)

instance (GenericFromBSON a) => GenericFromBSON (M1 D c a) where
    genericFromBSON doc = genericFromBSON doc >>= return . M1

instance (Val a, Selector s) => GenericFromBSON (S1 s (K1 i a)) where
    genericFromBSON doc = (BSON.lookup sname doc) >>= return . M1 . K1
        where sname = u . selName $ (undefined :: S1 s (K1 i a) r)

-- | ObjectKey special treatment
instance (Selector s) => GenericFromBSON (S1 s (K1 i ObjectKey)) where
    genericFromBSON doc = Just . M1 . K1 $ ObjectKey (BSON.lookup keyLabel doc)

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

data Comment = Comment { author :: String, comments :: [Comment] } deriving (Generic, Typeable, Show, Eq)
instance ToBSON Comment
instance FromBSON Comment
-- (fromBSON $ toBSON $ Comment "Joe1" [Comment "Joe2" [], Comment "Joe3" [Comment "Joe4" []]]) :: Maybe Comment

data Test4 = Test4 { test4Key :: ObjectKey, test4 :: String } deriving (Generic, Typeable, Show, Eq)
instance ToBSON Test4
instance FromBSON Test4
-- (fromBSON $ toBSON $ Test4 (unsafePerformIO genObjectId) "something") :: Maybe Test4
-- (fromBSON $ toBSON $ Test4 Nothing "something") :: Maybe Test4
-}