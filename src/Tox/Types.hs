module Tox.Types
       ( Sound
       , Track
       ) where

import FRP.Yampa

-- ^ Convenience type for unlimited tracks
type Sound = SF () Double

-- ^ Track type, parametrized with sample type (e.g. Int16), is essentially a signal function
-- returning samples and stop event.
type Track = SF () (Double, Event ())
