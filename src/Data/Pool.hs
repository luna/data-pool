module Data.Pool where

import Prelude
import Data.Default
import Data.Monoid
import Data.Container     hiding (free)
import Data.Container.List
import Control.Lens.Utils


-- === Declarations === --

newtype Pool a = Pool { _free :: [a] } deriving (Show, Functor, Foldable, Traversable)
makeClassy  ''Pool


-- === Utils === --

allocate :: Pool a -> (a, Pool a)
allocate (Pool (a : as)) = (a , Pool as)


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
