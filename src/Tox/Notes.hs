module Tox.Notes
       ( Note(..)
       , Octave
       , freq
       ) where

-- ^ All 12 notes of octave without ambiguities.
data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
          deriving (Eq, Show, Ord, Enum)

type Octave = Int

-- ^ Frequency of C in 0'th octave.
c0 :: Double
c0 = 16.352

-- ^ Get frequency of particular note in particular octave.
freq :: Note -> Octave -> Double
freq n o = c0 * (2 ** (fromIntegral o + (fromIntegral $ fromEnum n)/12))
