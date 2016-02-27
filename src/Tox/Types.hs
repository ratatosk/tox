module Tox.Types
       ( Sound
       , Track
       , Schedule
       , TList
       ) where

import FRP.Yampa

-- ^ Convenience type for unlimited tracks
type Sound = SF () Double

-- ^ Track type is essentially a signal function returning samples and stop event.
type Track = SF () (Double, Event ())

-- ^ Shedule describing sets of events/actions that happen with given time intervals.
type Schedule a t = [([a], t)]

-- ^ List of tracks with delays between them.
type TList = Schedule Track Time
