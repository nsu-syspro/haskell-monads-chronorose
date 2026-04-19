{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Hide built-in bind definition

import Data.Functor.Identity
import Prelude hiding ((>>=))

-- * Kleisli composition monad

-- | Monad based on Kleisli composition '(>=>)' operator
-- instead of usual bind operator '(>>=)'.
class (Applicative m) => KleisliMonad m where
  infixr 1 >=>
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

-- * Equivalent views

infixl 1 >>=

(>>=) :: (KleisliMonad m) => m a -> (a -> m b) -> m b
(>>=) ma f = (id >=> f) ma

join :: (KleisliMonad m) => m (m a) -> m a
join = id >=> id

-- * Instances

instance KleisliMonad Identity where
  (>=>) :: (a -> Identity b) -> (b -> Identity c) -> (a -> Identity c)
  (>=>) fa fb = fb . runIdentity . fa

instance KleisliMonad Maybe where
  (>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
  (>=>) fa fb = \a -> join (fb <$> fa a) -- i already defined join in terms of kleisli composition so this is fine, right?

instance KleisliMonad [] where
  (>=>) :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
  (>=>) fa fb = concatMap fb . fa

instance (Monoid e) => KleisliMonad ((,) e) where
  (>=>) :: (Monoid e) => (a -> (e, b)) -> (b -> (e, c)) -> (a -> (e, c))
  (>=>) fa fb = \x -> case fa x of
    (e1, b) -> case fb b of
      (e2, c) -> (e1 <> e2, c)

instance KleisliMonad ((->) e) where
  (>=>) :: (a -> e -> b) -> (b -> e -> c) -> (a -> e -> c)
  (>=>) fa fb = \a -> \e -> fb (fa a e) e
