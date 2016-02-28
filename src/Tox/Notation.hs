{-# LANGUAGE TemplateHaskell #-}

module Tox.Notation
       ( PitchClass(..)
       , Octave
       , Duration
       , freq
       , Note(..)
       , mkNote
       , mkRest
       , applyBPM
       , fromDuration
       , toSchedule
       ) where

import Tox.Types

import Data.Ratio
import Language.Haskell.TH

import FRP.Yampa

-- TODO: Maybe Rest shouldn't be note? Rather have Either Note Rest for notation?

-- ^ All 12 notes of octave without ambiguities.
data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
          deriving (Eq, Show, Ord, Enum, Bounded)

type Octave = Int

-- ^ Frequency of C in 0'th octave.
c0 :: Double
c0 = 16.352

-- ^ Get frequency of particular note in particular octave.
freq :: PitchClass -> Octave -> Double
freq n o = c0 * (2 ** (fromIntegral o + (fromIntegral $ fromEnum n)/12))

type Duration = Ratio Int

data Note = Note PitchClass Octave Duration | Rest Duration
  deriving (Show)

len :: Note -> Duration
len (Note _ _ d) = d
len (Rest d) = d

mkNote :: (PitchClass, Octave) -> Duration -> [Note]
mkNote (p, o) d = [Note p o d]

mkRest :: Duration -> [Note]
mkRest d = [Rest d]

p :: [[Note]] -> [Note]
p = concat

toSchedule :: [[Note]] -> Schedule Note Duration
toSchedule [] = []
toSchedule (n:ns) = (n, len (head n)) : toSchedule ns

fromDuration :: Int -> Ratio Int -> Double
fromDuration b x = 240 * fromIntegral (numerator x) / (fromIntegral (denominator x) * fromIntegral b)

-- ^ Apply BPM specified for quarter note.
applyBPM :: Int -> Schedule a Duration -> Schedule a Time
applyBPM b = map (\(x, d) -> (x, fromDuration b d))
