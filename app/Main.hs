{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa

import Tox.Types
import Tox.IO
import Tox.Util
import Tox.Music

mkSine :: Double -> Sound
mkSine freq = proc () -> do
    t <- time -< ()
    returnA -< 0.5 * sin (t * freq * 2 * pi)

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

mkEnvelope :: [(Double, Double)] -> SF () (Double, Event ())
mkEnvelope ls = run 0 ls &&& after (sum $ map fst ls) ()
  where
    run prev [] = constant prev
    run prev ((len, lvl):rest) = linearPiece prev lvl len (run lvl rest)

envKey :: SF () (Double, Event ())
envKey = mkEnvelope [(0.02, 1), (0.08, 0.5), (0.5, 0.3), (0.4, 0)]

mkKeySine :: Double -> Track
mkKeySine freq = proc () -> do
    v <- mkSine freq -< ()
    (l, ev) <- envKey -< ()
    returnA -< (v * l, ev)

tracks :: [([Track], Time)]
tracks = [([mkKeySine 440], 0.5), ([mkKeySine 330], 0.5), ([mkKeySine 220], 1)] --, ([mkKeySine 220], 1.0)]

demoTrack :: Track
demoTrack = pPlay tracks

main :: IO ()
main = do
    toWav44k16b demoTrack "test.wav"
