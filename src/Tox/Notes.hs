{-# LANGUAGE TemplateHaskell #-}

module Tox.Notes where

import Tox.Notation
import Tox.NotesTH
import Data.Ratio

-- generates shortcuts for notes, pitches and rests for concise notation, e.g.:
-- [n4 a4, r4, n4' b4, r8''] :: [[Note]]
-- n1...n128 denote note of duration 1/1 to 1/128.
-- nX' and nX'' denote dotted and double dotted notes.
-- note should have a pitch as its argument:
-- a5 means A of 5th octave.
-- as5 means A# of 5th octave.
-- rests are just like notes (including dotted) , but they don't require pitch.

$(genRests)
$(genPitches)
$(genNotes)
