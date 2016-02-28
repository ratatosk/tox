{-# LANGUAGE Arrows #-}
module Tox.Envelope
       ( Envelope
       , mkADSR
       , simpleADSR
       ) where

import Tox.Types

import FRP.Yampa

-- TODO: make it more flexible: envelope = ADS + R where ADS and R are both SFs,
-- ADS is parametrized by duration and R is parametrized by level at which ADS was aborted.

-- ^ Envelope essentially is a function accepting desired duration and returning signal containing
-- envelope level and termination event.
type Envelope = Double -> SF () (Double, Event ())

linearPiece :: Double -> Double -> Double -> Sound -> Sound
linearPiece from to len rest = switch (run &&& after len ()) (const rest)
  where
    run = proc () -> do
        t <- time -< ()
        returnA -< from + (to - from) * (t / len)

mkEnvelope :: [(Double, Double)] -> SF () (Double, Event ())
mkEnvelope ls = run 0 ls &&& after (sum $ map fst ls) ()
  where
    run prev [] = constant prev
    run prev ((len, lvl):rest) = linearPiece prev lvl len (run lvl rest)

-- ^ Generate simple Attack-Decay-Sustain-Release envelope consisting of linear pieces, sustain
-- piece is constant.
mkADSR :: Double -> Double -> Double -> Double -> Double -> Envelope
mkADSR at al dt dl rt dur = mkEnvelope[(at, al), (dt, dl), (susDur, dl), (rt, 0)]
  where
    susDur = max 0 (dur - at - dt - rt)

-- ^ Simple example envelope
simpleADSR :: Envelope
simpleADSR = mkADSR 0.02 1 0.08 0.5 0.2
