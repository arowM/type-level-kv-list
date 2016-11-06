{-|
  This module supply a way to construct type safe key-value pair list
  and convenient operations for the type.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

module Data.TypeLevelKVList
  (
  -- * Constructors
  -- $setup
    NamedVal
  , namedVal
  , (:.)(..)
  , Null(..)

  -- * Operators
  , get
  , Lookup
  , keys
  , keys'

  -- * Misc
  , NamedList
  )
where

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- Constructors

{- $setup #constructors#
  We can create type level KV list as follows.

  >>> :set -XDataKinds -XTypeOperators
  >>> let sampleList = (namedVal "str" :: NamedVal String "foo") :. (namedVal 34 :: NamedVal Int "bar") :. Null
  >>> type SampleList = NamedVal String "foo" :. NamedVal Int "bar" :. Null
-}

{-| A value with type level key.
 -}
type NamedVal v key = (Proxy key, v)

{-| A convenient function to construct type level KV list.
 -}
namedVal :: v -> NamedVal v k
namedVal a = (Proxy, a)

{-| Type level list cons.
 -}
data a :. b = a :. b
  deriving (Typeable, Eq, Show)
infixr 8 :.

{-| Type level empty list.
 -}
data Null = Null
  deriving (Typeable, Eq, Show)

{-| Type level @lookup :: k -> [(k, a)] -> a@.
 -}
type family Lookup pkey list where
  Lookup pk ((pk, v) :. b) = v
  Lookup pk ((px, v) :. b) = Lookup pk b
  Lookup pk Null = Null

{-| Main class for a list of values with type level key
 -}
class NamedList layout where
  type family NamedList' layout
  {-|
    >>> keys (Proxy :: Proxy SampleList)
    ["foo","bar"]
  -}
  keys :: Proxy layout -> [String]

instance (NamedList b, KnownSymbol k) => NamedList (NamedVal v k :. b) where
  type NamedList' (NamedVal v k :. b) = NamedVal v k :. NamedList' b
  keys _ = symbolVal (Proxy :: Proxy k) : keys (Proxy :: Proxy b)

instance NamedList Null where
  type NamedList' Null = Null
  keys _ = []

{-| Chek if the key is included in type level list.
 -}
class HasKey list pkey value where
  get' :: pkey -> list -> value

instance HasKey (NamedVal v k :. b) (Proxy k) v where
  get' _ ((_, a) :. _) = a

instance HasKey b (Proxy k) v =>
  HasKey (a :. b) (Proxy k) v where
  get' p (_ :. b) = get' p b

instance HasKey Null (Proxy k) Null where
  get' _ Null = Null

{-|
  >>> get (Proxy :: Proxy "foo") sampleList
  "str"

  >>> get (Proxy :: Proxy "bar") sampleList
  34

  >>> get (Proxy :: Proxy "baz") sampleList
  Null
-}
get :: (HasKey list pkey (Lookup pkey list))
    => pkey -> list -> Lookup pkey list
get (pkey :: pkey) (list :: list) =
  get' pkey list :: Lookup pkey list

{-|
  >>> keys' sampleList
  ["foo","bar"]
-}
keys' :: NamedList k => k -> [String]
keys' (_ :: k) = keys (Proxy :: Proxy k)
