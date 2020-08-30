module Gammoids.Combinatorics.NChooseM exposing
    ( iterator
    , IterNChooseM
    )

{-| This module provides an iterator that iterates all subsets of
a ground set consisting of consecutive Int's 0, .., (n-1)
that have size m in reverse-lexicographic order.

@docs iterator

-}

import Array exposing (Array)
import Iter exposing (Iter)
import Maybe


{-| Gets the smallest ordered subset of enumeration indexes possible for given size.
-}
smallestOrderedSubset : Int -> Array Int
smallestOrderedSubset m =
    Array.initialize m (\x -> x)


{-| Gets the biggest ordered subset possible for given size and number of elements.
-}
biggestOrderedSubset : Int -> Int -> Array Int
biggestOrderedSubset n m =
    Array.initialize m (\x -> n - m + x)


{-| state needed for iterator protocol
-}
type alias OssState =
    { nextElement : Maybe (Array Int)
    , n : Int
    }


{-| create state from next element and size (encapsulated)
-}
createState : Maybe (Array Int) -> Int -> OssState
createState =
    OssState


{-| checks whether this ordered subset is the last one there is
-}
isBiggestOrderedSubset : Int -> Array Int -> Bool
isBiggestOrderedSubset n subset =
    let
        m =
            Array.length subset
    in
    subset == biggestOrderedSubset n m


{-| returns the first index in the array which is bigger than the first parameter
and for which the next element in the array is at least 2 bigger.
-}
incrementIndexAfter : Int -> Array Int -> Int
incrementIndexAfter i subset =
    case Array.get (i + 1) subset of
        Maybe.Nothing ->
            i

        -- last element in list
        Maybe.Just x ->
            case Array.get i subset of
                Maybe.Nothing ->
                    -- This branch is unreachable as Array.get (i+1) is not Nothing.
                    i

                Maybe.Just y ->
                    if y + 1 < x then
                        i

                    else
                        incrementIndexAfter (i + 1) subset


{-| returns the index that has to be incremented in order to get the next ordered subset.
-}
incrementIndex : Array Int -> Int
incrementIndex =
    incrementIndexAfter 0


{-| increments the appropriate index, sets the preceding indexes to their respective minimal value,
keeps the rest; this yields the next bigger ordered subset.
-}
nextOrderedSubset : Array Int -> Array Int
nextOrderedSubset subset =
    let
        i =
            incrementIndex subset

        xi =
            case Array.get i subset of
                Maybe.Nothing ->
                    -- only possible if subset is empty
                    0

                Maybe.Just x ->
                    x + 1

        m =
            Array.length subset
    in
    Array.initialize m
        (\idx ->
            if idx == i then
                xi

            else if idx < i then
                idx

            else
                case Array.get idx subset of
                    Maybe.Nothing ->
                        -- This branch is unreachable due to construction.
                        0

                    Maybe.Just y ->
                        y
        )


{-| compute the next smallest ordered subset from the current one
-}
nextSmallestOrderedSubset : OssState -> OssState
nextSmallestOrderedSubset state =
    let
        nextOss =
            state.nextElement
    in
    case nextOss of
        Maybe.Nothing ->
            -- finished iteration
            state

        Maybe.Just subset ->
            if isBiggestOrderedSubset state.n subset then
                createState Maybe.Nothing state.n

            else
                createState (Maybe.Just (nextOrderedSubset subset)) state.n


{-| compute the next smallest ordered subset and return the current one
-}
next : OssState -> Maybe ( OssState, Array Int )
next state =
    case state.nextElement of
        Maybe.Nothing ->
            Maybe.Nothing

        Maybe.Just x ->
            Maybe.Just ( nextSmallestOrderedSubset state, x )


{-| iterator type alias
-}
type alias IterNChooseM =
    Iter (Array Int) OssState


{-| Get an iterator of all ordered subsets with fixed size in reverse-lexicographic ordering.
-}
iterator : Int -> Int -> IterNChooseM
iterator n m =
    if n >= m then
        Iter.iterator next (createState (Maybe.Just (smallestOrderedSubset m)) n)

    else
        Iter.iterator next (createState Maybe.Nothing n)
