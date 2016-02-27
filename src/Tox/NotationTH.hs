{-# LANGUAGE TemplateHaskell #-}

module Tox.NotationTH where

import Language.Haskell.TH

import Data.Char
import Data.Ratio

import Tox.Notation

mkDurationName :: String -> Integer -> Int -> (Name, Integer, Integer)
mkDurationName c i d =
  let n = mkName (c ++ show i ++ replicate d '\'')
      nom = 2 ^ (d+1) - 1
      denom = i * 2 ^ d
  in (n, nom, denom)

merge :: Monad m => [m [a]] -> m [a]
merge = fmap concat . sequence

genRest :: Integer -> Int -> Q [Dec]
genRest i d =
  let (n, nom, denom) = mkDurationName "r" i d
  in [d|$(varP n) = mkRest ($(litE $ IntegerL nom) % $(litE $ IntegerL denom))|]

genRests :: Q [Dec]
genRests = merge [genRest (2^i) d | i <- [0,1,2,3,4,5,6,7], d <- [0,1,2]]

genPitch :: PitchClass -> Integer -> Q [Dec]
genPitch pc o =
  let cn = mkName $ show pc
      n = mkName $ map toLower (show pc) ++ show o
  in [d|$(varP n) = ($(conE cn), $(litE $ IntegerL o)::Int)|]

genPitches :: Q [Dec]
genPitches = merge [genPitch pc o | pc <- [minBound..maxBound], o <- [0..10]]

genNote :: Integer -> Int -> Q [Dec]
genNote i d =
  let (n, nom, denom) = mkDurationName "n" i d
  in [d|$(varP n) = \p -> mkNote p ($(litE $ IntegerL nom) % $(litE $ IntegerL denom))|]

genNotes :: Q [Dec]
genNotes = merge [genNote (2^i) d | i <- [0,1,2,3,4,5,6,7], d <- [0,1,2]]
