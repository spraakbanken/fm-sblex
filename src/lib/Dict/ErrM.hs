-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module Dict.ErrM where

-- the Error monad: like Maybe type with error msgs

import qualified Control.Monad as CM (MonadPlus(..), liftM,ap)
import Control.Applicative (Applicative(..),Alternative(..))

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

instance Applicative Err where
        pure = return
        (<*>) = CM.ap

instance Alternative Err where
    empty = Bad "Err.empty"
    Bad _ <|> r = r
    l   <|> _ = l

instance Functor Err where
  fmap = CM.liftM

instance CM.MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x
