{-# LANGUAGE GADTs, TypeOperators, ConstraintKinds, FlexibleContexts #-}

-- This is working thru Conal Elliott's blog post "Reimagining Matrices"
-- http://conal.net/blog/posts/reimagining-matrices

import Data.VectorSpace
import Control.Arrow ((***), (&&&))

infix  7 :&&
infixr 1 :-*

type a :* b = (a, b)

data a :-* b where
   Dot :: InnerSpace b => b -> (b :-* Scalar b)
   (:&&) :: VS3 a c d => (a :-* c) -> (a :-* d) -> (a :-* c :* d)
   

-- other stuff

type VS a = (InnerSpace a, HasZero a, HasScale a, Num (Scalar a))

type a :~ b = Scalar a ~ Scalar b

type CV b a = (VS b, b :~ a)  -- compatible vector space

type VS2 a b     = (VS a     , CV b a)
type VS3 a b c   = (VS2 a b  , CV c a)
type VS4 a b c d = (VS3 a b c, CV d a)

class VectorSpace v => HasScale v where
  scale :: Scalar v -> v :-* v

idL :: (HasScale v, Num (Scalar v)) => v :-* v
idL = scale 1

instance VS2 a b => HasScale (a, b) where
  scale s = scale s *** scale s

class HasZero z where zeroL :: CV a z => a :-* z

instance VS2 a b => HasZero (a, b) where
  zeroL = zeroL &&& zeroL

instance HasZero  (Int) where { zeroL = Dot zeroV }
instance HasScale (Int) where scale = Dot

instance HasZero  (Integer) where { zeroL = Dot zeroV }
instance HasScale (Integer) where scale = Dot

instance HasZero  (Float) where { zeroL = Dot zeroV }
instance HasScale (Float) where scale = Dot

instance HasZero  (Double) where { zeroL = Dot zeroV }
instance HasScale (Double) where scale = Dot
