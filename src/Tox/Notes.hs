{-# LANGUAGE TemplateHaskell #-}

module Tox.Notes where

import Tox.Notation
import Tox.NotesTH
import Data.Ratio

$(genRests)
$(genPitches)
$(genNotes)
