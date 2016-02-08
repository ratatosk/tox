module Tox.IO
       ( render
       , toWav44k16b
       ) where

import Tox.Types

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
