{-# LANGUAGE Arrows #-}

module Tox.Music where

import Tox.Types
import Tox.Util

import Data.Ratio

import FRP.Yampa

type Duration = Ratio Int

type TList = [([Track], Time)]

-- TODO: get rid of keepAlive and rename it all

infixr 5 |@|
infixr 6 -@-
class Music m where
  (-@-) :: m -> m -> m
  (|@|) :: m -> m -> m
  len :: m -> Duration

finishes :: SF [(Double, Event ())] [Bool]
finishes = arr $ map (isEvent . snd)

checkStart :: [Time] -> Time -> ([Time], Bool)
checkStart [] _ = ([], False)
checkStart (d:ds) e = if e >= d then (ds, True) else ((d - e):ds, False)

type Change = ([Bool], Bool)

triggerInt :: [Time] -> Time -> [Bool] -> ([Time], Change)
triggerInt ds el fns =
  let (nds, st) = checkStart ds el
  in (nds, (fns, st))

trigger :: [Time] -> SF ((), [(Double, Event ())]) (Event ([Time], Change))
trigger tl = proc (_, cur) -> do
  f <- finishes -< cur
  el <- localTime -< ()
  ev <- iEdge False -< or f || (not (null tl) && el >= head tl)
  returnA -< tag ev (triggerInt tl el f)

filterFinished :: [a] -> [Bool] -> [a]
filterFinished l f = map fst $ filter snd $ zip l (map not f)

keepAlive :: [Track] -> Bool -> [Track]
keepAlive [] True = [limit 1 $ constant 0]
keepAlive t _ = t

safeCut :: [[a]] -> ([a], [[a]])
safeCut [] = ([], [])
safeCut (x:xs) = (x, xs)

update :: [[Track]] -> [Track] -> ([Time], Change) -> SF () [(Double, Event ())]
update tlist conts (ds, (finishes, start)) = pSwitchB next (trigger ds) (update rest)
  where
    alive = filterFinished conts finishes
    (born, rest) = if start
      then safeCut tlist
      else ([], tlist)
    next = keepAlive (alive ++ born) (not $ null rest)

pMerge :: TList -> SF () [(Double, Event ())]
pMerge t = update ts [] (0:ds, noChange)
  where
    (ts, ds) = unzip t
    noChange = ([], False)

pMix :: SF [(Double, a)] (Double, Event ())
pMix = proc l -> do
  ev <- iEdge False -< null l
  returnA -< (sum (map fst l), ev)

pPlay :: TList -> Track
pPlay t = pMerge t >>> pMix
