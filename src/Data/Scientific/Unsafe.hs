{-# LANGUAGE Unsafe #-}

module Data.Scientific.Unsafe
    ( unsafeScientificFromNormalized
    , unsafeScientificFromNonNormalized
    , unsafeAddScientific
    , unsafeSubcientific
    , unsafeToRational
    , unsafeRecip
    , unsafeDivScientific
    , differentlyUnsafeFromRational
    , unsafeFromRational
    , unsafeToRationalRepetend
    , unsafeProperFraction
    , unsafeTruncate
    , unsafeRound
    , unsafeCeiling
    , unsafeFloor
    ) where

import Data.Scientific.Internal
