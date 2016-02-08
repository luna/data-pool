module Data.Pool where

import Prelude
import Control.Lens
import Data.Default
import Data.Monoid
import Data.Container     hiding (free)
import Data.Container.List
import Control.Lens.Utils


-- === Declarations === --

data Pool a = Pool { _free :: [a] } deriving (Show, Functor, Foldable, Traversable)
makeClassy  ''Pool
makeWrapped ''Pool


-- === Instances === --

-- Primitive

instance Default a => Default (Pool a) where def = Pool def ; {-# INLINE def #-}

instance Monoid (Pool a) where
    mempty                    = Pool mempty   ; {-# INLINE mempty  #-}
    mappend (Pool a) (Pool b) = Pool $ a <> b ; {-# INLINE mappend #-}

-- Container

type instance Item (Pool a) = a
instance  FromList (Pool a) where fromList = wrap'   ; {-# INLINE fromList #-}
instance  ToList   (Pool a) where toList   = unwrap' ; {-# INLINE toList   #-}

