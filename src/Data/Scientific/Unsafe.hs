{-# LANGUAGE Unsafe #-}

module Data.Scientific.Unsafe
    ( unsafeScientificFromNormalized
    , unsafeScientificFromNonNormalized
    , unsafeAddScientific
    , unsafeSubScientific
    , unsafeToRational
    , unsafeTruncate
    , unsafeProperFraction
    , unsafeDivScientific
    , unsafeRecip
    , unsafeRound
    , unsafeCeiling
    , unsafeFloor
    , unsafeRealToScientific
    , unsafeScientificToFrac
    , differentlyUnsafeFromRational
    , unsafeFromRational
    , unsafeToRationalRepetend
    ) where

import Data.Scientific.Internal
