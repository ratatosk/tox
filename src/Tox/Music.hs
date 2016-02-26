module Tox.Music where

import Data.Ratio

type Duration = Ratio Int

infixr 5 |@|
infixr 6 -@-
class Music m where
  (-@-) :: m -> m -> m
  (|@|) :: m -> m -> m
  len :: m -> Duration
