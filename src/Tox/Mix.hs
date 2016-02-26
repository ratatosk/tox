{-# LANGUAGE Arrows #-}

module Tox.Mix
       ( mix
       ) where

import Tox.Types
import Tox.Util

import FRP.Yampa

type TList = [([Track], Time)]

-- TODO: get rid of keepAlive and rename it all

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

-- ^ Generates an event when either some tracks end or when it is time to add another one to the
-- mix. Used for triggering behavior switching.
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

-- ^ Main worker function: run tracks in parallel switching behaviors when one of the tracks ends or
-- when it is time to add another one to the mix.
update :: [[Track]] -> [Track] -> ([Time], Change) -> SF () [(Double, Event ())]
update tlist conts (ds, (finishes, start)) = pSwitchB next (trigger ds) (update rest)
  where
    alive = filterFinished conts finishes
    (born, rest) = if start
      then safeCut tlist
      else ([], tlist)
    next = keepAlive (alive ++ born) (not $ null rest)

-- ^ Run tracks from list in parallel with appropriate delays.
pRun :: TList -> SF () [(Double, Event ())]
pRun t = update ts [] (0:ds, noChange)
  where
    (ts, ds) = unzip t
    noChange = ([], False)

-- ^ Sum tracks into one. Track ends when list becomes empty.
pMerge :: SF [(Double, a)] (Double, Event ())
pMerge = proc l -> do
  ev <- iEdge False -< null l
  returnA -< (sum (map fst l), ev)

mix :: TList -> Track
mix t = pRun t >>> pMerge
