{-# LANGUAGE Arrows #-}

module Tox.Synth where

import Tox.Types
import Tox.Notation
import Tox.Envelope
import Tox.Mix

import Control.Arrow

import FRP.Yampa

-- TODO: Remove Rest from Note.
-- TODO: Move Oscillator to separate module.

-- ^ Synth makes track from frequency and duration
type Synth = Double -> Double -> Track

-- ^ Oscillator generates unbound sound for given frequency
type Oscillator = Double -> Sound

mkSynth :: Oscillator -> Envelope -> Synth
mkSynth o e f d = proc () -> do
  oscVal <- o f -< ()
  (envVal, termEv) <- e d -< ()
  returnA -< (oscVal * envVal, termEv)

mapSchedule :: (a -> b) -> Schedule a c -> Schedule b c
mapSchedule f = map (\(x, d) -> (map f x, d))

synth :: Synth -> Int -> Schedule Note Duration -> Track
synth s bpm sched =
  let isNote (Note _ _ _) = True
      isNote (Rest _) = False
      filterNotes (ns, d) = (filter isNote ns, d)
      nSched = map filterNotes sched
      toFreqDur (Note p o d) = (freq p o, fromDuration bpm d)
      tSched = applyBPM bpm (mapSchedule toFreqDur nSched)
      synthNote (f, d) = s f d
      synthNotes (ns, d) = (map synthNote ns, d)
  in mix $ map synthNotes tSched

sine :: Oscillator
sine freq = proc () -> do
    t <- time -< ()
    returnA -< 0.5 * sin (t * freq * 2 * pi)

simpleSynth :: Synth
simpleSynth = mkSynth sine simpleADSR
