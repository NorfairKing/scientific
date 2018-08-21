{-# LANGUAGE Unsafe #-}

module Data.Scientific.Unsafe
    ( unsafeFromNormalized
    , unsafeFromNonNormalized
    , unsafeAdd
    , unsafeSub
    , unsafeToRational
    , unsafeTruncate
    , unsafeProperFraction
    , unsafeDiv
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
