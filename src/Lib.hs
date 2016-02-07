{-# LANGUAGE FlexibleContexts #-}

module Lib
       ( Sound
       , Track
       , Sample(..)
       , render
       , toWav44k16b
       , limit
       , switchAfter
       , glue
       ) where


import Control.Monad
import Control.Applicative

import Data.Int
import Data.IORef
import Data.Array.Unboxed
import Data.Audio

import FRP.Yampa
import FRP.Yampa.Simulation
import Codec.Wav
import System.IO

-- TODO: add type-driven support for multiple channels.
-- TODO: make bit depth adjustable
-- TODO: add fast arraybuilder
-- TODO: count clipping by square, not number of samples.

-- ^ Class for quantized representation.
class Quantized a where
    -- ^ Convert Sample to quantized value, where Right a means that sample was represented
    -- correctly and Left a means that sample was clipped.
    quantize :: Double -> (a, Bool)

-- ^ Minimum quantization level for Int16
minLevel16 :: (Int16, Bool)
minLevel16 = (fromSample (-0.999), True)
-- ^ Maximum quantization level for Int16
maxLevel16 :: (Int16, Bool)
maxLevel16 = (fromSample 0.999, True)

instance Quantized Int16 where
    quantize x | x > 0.999 = maxLevel16
               | x < -0.999 = minLevel16
               | otherwise = (fromSample x, False)

-- ^ Convenience type for unlimited tracks
type Sound = SF () Double

-- ^ Track type, parametrized with sample type (e.g. Int16), is essentially a signal function
-- returning samples and stop event.
type Track = SF () (Double, Event ())

-- ^ Render track to saple data.
render :: Track -> Int -> IO (Audio Int16, Double)
render track sampleRate = do
    samples <- newIORef []
    numSamples <- newIORef 0
    numClipped <- newIORef 0
    let quant = 1.0 / fromIntegral sampleRate
        sense = const $ return (quant, Nothing)
        actuate _ (_, Event e) = return True
        actuate _ (s,_) = do
            let (qs, clipped) = quantize s
            modifyIORef samples (qs:)
            modifyIORef' numSamples (+1)
            when clipped $ modifyIORef' numClipped (+1)
            return False
    reactimate (return ()) sense actuate track
    finalSamples <- reverse <$> readIORef samples
    finalNumSamples <- readIORef numSamples
    finalNumClipped <- readIORef numClipped
    let sampleArray = listArray (0, (finalNumSamples-1)) finalSamples
        clipRatio = fromIntegral finalNumClipped / fromIntegral finalNumSamples
    return (Audio sampleRate 1 sampleArray, clipRatio)

-- ^ Write track to 44100Hz 16 bit wav file.
toWav44k16b :: Track -> FilePath -> IO ()
toWav44k16b t p = do
    (wavData, clipRatio) <- render t 44100
    putStrLn $ "Clipped ratio: " ++ show clipRatio
    exportFile p wavData

-- ^ Convenience function to limit sound length
limit :: Double -> Sound -> Track
limit d s = s &&& after d ()

-- ^ Convenience funciton to switch to other sound after some time
switchAfter :: Double -> Sound -> Sound -> Sound
switchAfter delay former latter = switch (former &&& after delay ()) (const latter)

-- ^ Convenience function to switch through list of sounds with specified lengths
glue :: [(Double, Sound)] -> Sound
glue [] = constant 0
glue ((len, snd):rest) = switchAfter len snd (glue rest)
