{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa

import Lib

mkSine :: Double -> Sound
mkSine freq = proc () -> do
    t <- time -< ()
    returnA -< sin (t * freq * 2 * pi)

demoSine2 :: Sound
demoSine2 = proc () -> do
    t <- time -< ()
    phase <- integral -< (440 * 2 * pi) * (1 + 0.8 * sin (t * 2 * pi))
    returnA -< sin phase

demoData :: Sound
demoData = switch (mkSine 440 &&& after 1 ()) (const $ mkSine 660)

linearPiece :: Double -> Double -> Double -> Sound -> Sound
linearPiece from to len rest = switch (run &&& after len ()) (const rest)
  where
    run = proc () -> do
        t <- time -< ()
        returnA -< from + (to - from) * (t / len)

mkEnvelope :: [(Double, Double)] -> Sound
mkEnvelope ls = run 0 ls
  where
    run prev [] = constant prev
    run prev ((len, lvl):rest) = linearPiece prev lvl len (run lvl rest)

envKey :: Sound
envKey = mkEnvelope [(0.02, 1), (0.08, 0.5), (0.5, 0.3), (0.4, 0)]

mkKeySine :: Double -> Sound
mkKeySine freq = proc () -> do
    v <- mkSine freq -< ()
    e <- envKey -< ()
    returnA -< v * e

demoTrack :: Track
demoTrack = demoData &&& after 10 ()

demoTrack2 :: Track
demoTrack2 = limit 10 $ glue [(1, constant 0), (1, mkKeySine 440), (1, mkKeySine 660)]

main :: IO ()
main = do
    toWav44k16b demoTrack2 "test.wav"