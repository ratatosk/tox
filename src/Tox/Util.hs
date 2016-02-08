module Tox.Util
       ( limit
       , switchAfter
       , glue
       ) where

import Tox.Types

import Control.Arrow

import FRP.Yampa

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
