module Solver exposing (..)

import Types exposing (..)
-- shortest path solver;
-- all path searching happen in a limited circumference.
-- at the beginning scan walehouse within 1km,
-- keep scanning with a factor of 1.5
-- until it hits the boundary


type Proximity = Proximity Float Float Float



-- scan for sink surround the source marker
scanForSink : Marker -> List Marker -> Maybe (List Marker)
