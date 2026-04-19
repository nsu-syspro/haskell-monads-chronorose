{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

-- Hide built-in bind definition

import Data.Functor.Identity
import Prelude hiding ((>>=))

-- * Join monad

-- | Monad based on 'join' operation
-- instead of usual bind operator '(>>=)'.
class (Applicative m) => JoinMonad m where
  join :: m (m a) -> m a

-- * Equivalent views

infixl 1 >>=

(>>=) :: (JoinMonad m) => m a -> (a -> m b) -> m b
(>>=) m f = join (f <$> m)

infixr 1 >=>

(>=>) :: (JoinMonad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) fa fb = \a -> join (fb <$> fa a)

-- * Instances

instance JoinMonad Identity where
  join :: Identity (Identity a) -> Identity a
  join = runIdentity

instance JoinMonad Maybe where
  join :: Maybe (Maybe a) -> Maybe a
  join (Just x) = x
  join Nothing = Nothing

instance JoinMonad [] where
  join :: [[a]] -> [a]
  join = concat

instance (Monoid e) => JoinMonad ((,) e) where
  join :: (Monoid e) => (e, (e, a)) -> (e, a)
  join (e1, (e2, a)) = (e1 <> e2, a)

instance JoinMonad ((->) e) where
  join :: (e -> (e -> a)) -> (e -> a)
  join f = \e -> f e e
