module Gammoids exposing (n_choose_m)

{-| This is the main module of the 'gammoids and related combinatorics library'.
Here, we have some nice short-cuts to use.

    @docs n_choose_m

-}

import Gammoids.Combinatorics.NChooseM


{-| returns an iterator for all m-elementary subsets of the set {0,1,..,n-1}

See also: Iter.Iter

-}
n_choose_m : Int -> Int -> Gammoids.Combinatorics.NChooseM.IterNChooseM
n_choose_m =
    Gammoids.Combinatorics.NChooseM.iterator
