{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa

import Tox.Types
import Tox.IO
import Tox.Util
import Tox.Synth
import Tox.Notation
import Tox.Notes

demoTrack :: Track
demoTrack = synth simpleSynth 95 (toSchedule m) >>> first (arr (*0.5))
  where
    m = [n4 a3, n8 c5 ++ n8 e5, n8 b3, n4 f3, n8 c5 ++ n8 f5, n8 b3]
     ++ [n4 c4, n8 c5 ++ n8 e5, n8 g4, n8 e4, n8 d4, n8 e5 ++ n8 b4 ++ n8 gs4 ++ n8 c4, n8 b3]
     ++ [n4 a3, n8 c5 ++ n8 e5, n8 b3, n4 f3, n8 c5 ++ n8 f5, n8 b3]

main :: IO ()
main = do
    toWav44k16b demoTrack "test.wav"
