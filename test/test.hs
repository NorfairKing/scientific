{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import           Data.Int
import           Data.Word
import           Data.Scientific                    as Scientific
import           Data.Scientific.Unsafe             as Scientific
import           Test.Tasty
import           Test.Tasty.Runners.AntXML
import           Test.Tasty.HUnit                          (testCase, (@?=), Assertion, assertBool)
import qualified Test.SmallCheck                    as SC
import qualified Test.SmallCheck.Series             as SC
import qualified Test.Tasty.SmallCheck              as SC  (testProperty)
import qualified Test.QuickCheck                    as QC
import qualified Test.Tasty.QuickCheck              as QC  (testProperty)
-- import qualified Data.Binary                        as Binary (encode, decode)
import qualified Data.Text.Lazy                     as TL  (unpack)
import qualified Data.Text.Lazy.Builder             as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Scientific  as T
import           Numeric ( floatToDigits )

import qualified Data.ByteString.Lazy.Char8         as BLC8
import qualified Data.ByteString.Builder.Scientific as B
import qualified Data.ByteString.Builder            as B
import           Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = testMain $ testGroup "scientific"
  [ testGroup "DoS protection"
    [ testGroup "Eq"
      [ testCase "1e1000000" $ assertBool "" $
          (read "1e1000000" :: Scientific) == (read "1e1000000" :: Scientific)
      ]
    , testGroup "Ord"
      [ testCase "compare 1234e1000000 123e1000001" $
          compare (read "1234e1000000" :: Scientific) (read "123e1000001" :: Scientific) @?= GT
      ]

    , testGroup "RealFrac replacements"
      [ testGroup "unsafeFloor"
        [ testCase "1e1000000"   $ (unsafeFloor (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (unsafeFloor (read "-1e-1000000" :: Scientific) :: Int) @?= (-1)
        , testCase "1e-1000000"  $ (unsafeFloor (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "unsafeCeiling"
        [ testCase "1e1000000"   $ (unsafeCeiling (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (unsafeCeiling (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (unsafeCeiling (read "1e-1000000"  :: Scientific) :: Int) @?= 1
        ]
      , testGroup "unsafeRound"
        [ testCase "1e1000000"   $ (unsafeRound (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (unsafeRound (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (unsafeRound (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "unsafeTruncate"
        [ testCase "1e1000000"   $ (unsafeTruncate (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (unsafeTruncate (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (unsafeTruncate (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "unsafeProperFracton"
        [ testCase "1e1000000"   $ unsafeProperFraction (read "1e1000000" :: Scientific) @?= (0 :: Int, zeroScientific)
        , testCase "-1e-1000000" $ let s = read "-1e-1000000" :: Scientific
                                   in unsafeProperFraction s @?= (0 :: Int, s)
        , testCase "1e-1000000"  $ let s = read "1e-1000000" :: Scientific
                                   in unsafeProperFraction s @?= (0 :: Int, s)
        ]
      ]
    , testGroup "toRealFloat"
      [ testCase "1e1000000"  $ assertBool "Should be infinity!" $ isInfinite $
                                  (toRealFloat (read "1e1000000" :: Scientific) :: Double)
      , testCase "1e-1000000" $ (toRealFloat (read "1e-1000000" :: Scientific) :: Double) @?= 0
      ]
    , testGroup "toBoundedInteger"
      [ testCase "1e1000000"  $ (toBoundedInteger (read "1e1000000" :: Scientific) :: Maybe Int) @?= Nothing
      ]
    ]

  , smallQuick "normalization"
       (\s -> s /= zeroScientific SC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)
       (\s -> s /= zeroScientific QC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)

--   , testGroup "Binary"
--     [ testProperty "decode . encode == id" $ \s ->
--         Binary.decode (Binary.encode s) === s
--     ]

  , testGroup "Parsing"
    [ testCase "reads \"\""        $ testReads ""        []
    , testCase "reads \"1.\""      $ testReads "1."      [(scientific 10 (-1), ".")]
    , testCase "reads \"1.2e\""    $ testReads "1.2e"    [(scientific 12 (-1), "e")]
    , testCase "reads \"(1.3 )\""  $ testReads "(1.3 )"  [(scientific 13 (-1), "")]
    , testCase "reads \"((1.3))\"" $ testReads "((1.3))" [(scientific 13 (-1), "")]
    , testCase "reads \" 1.3\""    $ testReads " 1.3"    [(scientific 13 (-1), "")]
    , testCase "read \" ( ((  -1.0e+3 ) ))\"" $ testRead " ( ((  -1.0e+3 ) ))" (scientific (-1) 3)
    , testCase "scientificP \"3\""       $ testScientificP "3"       [(scientific 3 0, "")]
    , testCase "scientificP \"3.0e2\""   $ testScientificP "3.0e2"   [(scientific 3 0, "e2"), (scientific 3 2, "")]
    , testCase "scientificP \"+3.0e+2\"" $ testScientificP "+3.0e+2" [(scientific 3 0, "e+2"), (scientific 3 2, "")]
    , testCase "scientificP \"-3.0e-2\"" $ testScientificP "-3.0e-2" [(scientific (-3) 0, "e-2"), (scientific (-3) (-2), "")]
    ]

  , testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === s

    , testGroup "toDecimalDigits"
      [ smallQuick "laws"
          (SC.over   nonNegativeScientificSeries toDecimalDigits_laws)
          (QC.forAll nonNegativeScientificGen    toDecimalDigits_laws)

      , smallQuick "== Numeric.floatToDigits"
          (toDecimalDigits_eq_floatToDigits . SC.getNonNegative)
          (toDecimalDigits_eq_floatToDigits . QC.getNonNegative)
      ]

    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          TL.unpack (TLB.toLazyText $
                       T.formatScientificBuilder Scientific.Generic Nothing s)

      , testProperty "ByteString" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          BLC8.unpack (B.toLazyByteString $
                        B.formatScientificBuilder Scientific.Generic Nothing s)
      ]

    , testProperty "formatScientific_fromFloatDigits" $ \(d::Double) ->
        formatScientific Scientific.Generic Nothing (Scientific.fromFloatDigits d) ==
        show d

    -- , testProperty "formatScientific_realToFrac" $ \(d::Double) ->
    --     formatScientific B.Generic Nothing (realToFrac d :: Scientific) ==
    --     show d
    ]

  , testGroup "Eq"
    [ testProperty "==" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        (s1 == s2) == (unsafeToRational s1 == unsafeToRational s2)
    , testProperty "s == s" $ \(s :: Scientific) -> s == s
    ]

  , testGroup "Ord"
    [ testProperty "compare" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        compare s1 s2 == compare (unsafeToRational s1) (unsafeToRational s2)
    ]

  , testGroup "Num"
    [ testGroup "Equal to Rational"
      [ testProperty "fromInteger" $ \i -> fromIntegerScientific i === unsafeFromRational (fromInteger i)
      , testProperty "+"           $ bin (+) unsafeAddScientific
      , testProperty "-"           $ bin (-) unsafeSubScientific
      , testProperty "*"           $ bin (*) mulScientific
      , testProperty "abs"         $ unary abs absScientific
      , testProperty "negate"      $ unary negate negateScientific
      , testProperty "signum"      $ unary signum signumScientific
      ]

    , testProperty "0 identity of +" $ \a -> a `unsafeAddScientific` zeroScientific === a
    , testProperty "1 identity of *" $ \a -> scientific 1 0 `mulScientific` a === a
    , testProperty "0 identity of *" $ \a -> zeroScientific `mulScientific` a === zeroScientific

    , testProperty "associativity of `unsafeAddScientific`"         $ \a b c -> a `unsafeAddScientific` (b `unsafeAddScientific` c) === (a `unsafeAddScientific` b) `unsafeAddScientific` c
    , testProperty "commutativity of `unsafeAddScientific`"         $ \a b   -> a `unsafeAddScientific` b       === b `unsafeAddScientific` a
    , testProperty "distributivity of `mulScientific` over `unsafeAddScientific`" $ \a b c -> a `mulScientific` (b `unsafeAddScientific` c) === (a `mulScientific` b) `unsafeAddScientific` (a `mulScientific` c)

    , testProperty "subtracting the addition" $ \x y -> (x `unsafeAddScientific` y) `unsafeSubScientific` y === x

    , testProperty "+ and negate" $ \x -> x `unsafeAddScientific` negateScientific x === zeroScientific
    , testProperty "- and negate" $ \x -> x `unsafeSubScientific` negateScientific x === x `unsafeAddScientific` x

    , smallQuick "abs . negate == id"
        (SC.over   nonNegativeScientificSeries $ \x -> absScientific (negateScientific x) === x)
        (QC.forAll nonNegativeScientificGen    $ \x -> absScientific (negateScientific x) === x)
    ]

  , testGroup "Real"
    [ testProperty "fromRational . toRational == id" $ \x ->
        (unsafeFromRational . unsafeToRational) x === x
    ]

  , testGroup "RealFrac Replacements"
    [ testGroup "Equal to Rational"
      [ testProperty "properFraction" $ \x ->
          let (n1::Integer, f1::Scientific) = unsafeProperFraction x
              (n2::Integer, f2::Rational)   = properFraction (unsafeToRational x)
          in (n1 == n2) && (f1 == unsafeFromRational f2)

      , testProperty "unsafeRound" $ \(x::Scientific) ->
          (unsafeRound x :: Integer) == round (unsafeToRational x)

      , testProperty "unsafeTruncate" $ \(x::Scientific) ->
          (unsafeTruncate x :: Integer) == truncate (unsafeToRational x)

      , testProperty "unsafeCeiling" $ \(x::Scientific) ->
          (unsafeCeiling x :: Integer) == ceiling (unsafeToRational x)

      , testProperty "unsafeFloor" $ \(x::Scientific) ->
          (unsafeFloor x :: Integer) == floor (unsafeToRational x)
      ]

    , testProperty "unsafeProperFraction_laws" properFraction_laws

    , testProperty "unsafeRound"    $ \s -> unsafeRound    s == roundDefault    s
    , testProperty "unsafeTruncate" $ \s -> unsafeTruncate s == truncateDefault s
    , testProperty "unsafeCeiling"  $ \s -> unsafeCeiling  s == ceilingDefault  s
    , testProperty "unsafeFloor"    $ \s -> unsafeFloor    s == floorDefault    s
    ]

  , testGroup "Conversions"
    [ testProperty "fromRationalRepetend" $ \(l, r) -> r ==
        (case fromRationalRepetend (Just l) r of
          Left (s, rr) -> unsafeToRational s + rr
          Right (s, mbRepetend) ->
            case mbRepetend of
              Nothing       -> unsafeToRational s
              Just repetend -> unsafeToRationalRepetend s repetend)

    , testGroup "Float"  $ conversionsProperties (undefined :: Float)
    , testGroup "Double" $ conversionsProperties (undefined :: Double)

    , testGroup "floatingOrInteger"
      [ testProperty "correct conversion" $ \s ->
            case floatingOrInteger s :: Either Double Int of
              Left  d -> d == toRealFloat s
              Right i -> i == fromInteger (coefficient s) * 10^(base10Exponent s)
      , testProperty "Integer == Right" $ \(i::Integer) ->
          (floatingOrInteger (fromIntegerScientific i) :: Either Double Integer) == Right i
      , smallQuick "Double == Left"
          (\(d::Double) -> genericIsFloating d SC.==>
             (floatingOrInteger (unsafeRealToScientific d) :: Either Double Integer) == Left d)
          (\(d::Double) -> genericIsFloating d QC.==>
             (floatingOrInteger (unsafeRealToScientific d) :: Either Double Integer) == Left d)
      ]
    , testGroup "toBoundedInteger"
      [ testGroup "correct conversion"
        [ testProperty "Int64"       $ toBoundedIntegerConversion (undefined :: Int64)
        , testProperty "Word64"      $ toBoundedIntegerConversion (undefined :: Word64)
        , testProperty "NegativeNum" $ toBoundedIntegerConversion (undefined :: NegativeInt)
        ]
      ]
    ]
  , testGroup "toBoundedRealFloat"
    [ testCase "0 * 10^1000 == 0" $
        toBoundedRealFloat (scientific 0 1000) @?= Right (0 :: Float)
    ]
  , testGroup "toBoundedInteger"
    [ testGroup "to Int64" $
      [ testCase "succ of maxBound" $
        let i = succ . fromIntegral $ (maxBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "pred of minBound" $
        let i = pred . fromIntegral $ (minBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "0 * 10^1000 == 0" $
          toBoundedInteger (scientific 0 1000) @?= Just (0 :: Int64)
      ]
    ]
  , testGroup "Predicates"
    [ testProperty "isFloating" $ \s -> isFloating s ==      genericScientificIsFloating s
    , testProperty "isInteger"  $ \s -> isInteger  s == not (genericScientificIsFloating s)
    ]
  ]

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients (antXMLRunner:defaultIngredients)

testReads :: String -> [(Scientific, String)] -> Assertion
testReads inp out = reads inp @?= out

testRead :: String -> Scientific -> Assertion
testRead inp out = read inp @?= out

testScientificP :: String -> [(Scientific, String)] -> Assertion
testScientificP inp out = readP_to_S Scientific.scientificP inp @?= out

genericIsFloating :: RealFrac a => a -> Bool
genericIsFloating a = fromInteger (floor a :: Integer) /= a

genericScientificIsFloating :: Scientific -> Bool
genericScientificIsFloating s = fromIntegerScientific (unsafeFloor s :: Integer) /= s

toDecimalDigits_eq_floatToDigits :: Double -> Bool
toDecimalDigits_eq_floatToDigits d =
    Scientific.toDecimalDigits (Scientific.fromFloatDigits d)
      == Numeric.floatToDigits 10 d

conversionsProperties :: forall realFloat.
                         ( RealFloat    realFloat
                         , QC.Arbitrary realFloat
                         , SC.Serial IO realFloat
                         , Show         realFloat
                         )
                      => realFloat -> [TestTree]
conversionsProperties _ =
  [
    -- testProperty "fromFloatDigits_1" $ \(d :: realFloat) ->
    --   Scientific.fromFloatDigits d === realToFrac d

    -- testProperty "fromFloatDigits_2" $ \(s :: Scientific) ->
    --   Scientific.fromFloatDigits (realToFrac s :: realFloat) == s

    testProperty "toRealFloat" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . unsafeRealToScientific) d == d

  , testProperty "toRealFloat . fromFloatDigits == id" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . Scientific.fromFloatDigits) d == d

  -- , testProperty "fromFloatDigits . toRealFloat == id" $ \(s :: Scientific) ->
  --     Scientific.fromFloatDigits (Scientific.toRealFloat s :: realFloat) == s
  ]

toBoundedIntegerConversion
    :: forall i. (Integral i, Bounded i)
    => i -> Scientific -> Bool
toBoundedIntegerConversion _ s =
    case toBoundedInteger s :: Maybe i of
      Just i -> i == (fromIntegral $ (coefficient s) * 10^(base10Exponent s)) &&
                i >= minBound &&
                i <= maxBound
      Nothing -> isFloating s ||
                 s < fromIntegralScientific (minBound :: i) ||
                 s > fromIntegralScientific (maxBound :: i)

testProperty :: (SC.Testable IO test, QC.Testable test)
             => TestName -> test -> TestTree
testProperty n test = smallQuick n test test

smallQuick :: (SC.Testable IO smallCheck, QC.Testable quickCheck)
             => TestName -> smallCheck -> quickCheck -> TestTree
smallQuick n sc qc = testGroup n
                     [ SC.testProperty "smallcheck" sc
                     , QC.testProperty "quickcheck" qc
                     ]

-- | ('==') specialized to 'Scientific' so we don't have to put type
-- signatures everywhere.
(===) :: Scientific -> Scientific -> Bool
(===) = (==)
infix 4 ===

bin :: (forall a. Num a => a -> a -> a) -> (Scientific -> Scientific -> Scientific) -> Scientific -> Scientific -> Bool
bin opN opS a b = unsafeToRational (a `opS` b) == unsafeToRational a `opN` unsafeToRational b

unary :: (forall a. Num a => a -> a) -> (Scientific -> Scientific) -> Scientific -> Bool
unary opN opS a = unsafeToRational (opS a) == opN (unsafeToRational a)

toDecimalDigits_laws :: Scientific -> Bool
toDecimalDigits_laws x =
  let (ds, e) = Scientific.toDecimalDigits x

      rule1 = n >= 1
      n     = length ds

      rule2 = unsafeToRational x == coeff * 10 ^^ e
      coeff = foldr (\di a -> a / 10 + fromIntegral di) 0 (0:ds)

      rule3 = all (\di -> 0 <= di && di <= 9) ds

      rule4 | n == 1    = True
            | otherwise = null $ takeWhile (==0) $ reverse ds

  in rule1 && rule2 && rule3 && rule4

properFraction_laws :: Scientific -> Bool
properFraction_laws x = fromIntegerScientific n `unsafeAddScientific` f === x &&
                        (positive n           == posX || n == 0) &&
                        (positiveScientific f == posX || f == zeroScientific) &&
                        absScientific f < scientific 1 0
    where
      posX = positiveScientific x

      (n, f) = unsafeProperFraction x :: (Integer, Scientific)

positive :: (Ord a, Num a) => a -> Bool
positive y = y >= 0

positiveScientific :: Scientific -> Bool
positiveScientific s = s >= zeroScientific

floorDefault :: Scientific -> Integer
floorDefault x = if r < zeroScientific then n - 1 else n
                 where (n,r) = unsafeProperFraction x

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > zeroScientific then n + 1 else n
                   where (n,r) = unsafeProperFraction x

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = unsafeProperFraction x

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = unsafeProperFraction x
                     m     = if r < zeroScientific then n - 1 else n + 1
                     sig   = signumScientific (absScientific r `unsafeSubScientific` scientific 5 (-1))
                 in  if
                   | sig == scientific (-1) 0 -> n
                   | sig == zeroScientific    -> if even n then n else m
                   | sig == scientific   1  0 -> m
                   | otherwise                -> error "round default defn: Bad value"

newtype NegativeInt = NegativeInt Int
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded NegativeInt where
    minBound = -100
    maxBound = -10

----------------------------------------------------------------------
-- SmallCheck instances
----------------------------------------------------------------------

instance (Monad m) => SC.Serial m Scientific where
    series = scientifics

scientifics :: (Monad m) => SC.Series m Scientific
scientifics = SC.cons2 scientific

nonNegativeScientificSeries :: (Monad m) => SC.Series m Scientific
nonNegativeScientificSeries = liftM absScientific SC.series


----------------------------------------------------------------------
-- QuickCheck instances
----------------------------------------------------------------------

instance QC.Arbitrary Scientific where
    arbitrary = QC.frequency
      [ (70, scientific <$> QC.arbitrary
                        <*> intGen)
      , (20, scientific <$> QC.arbitrary
                        <*> bigIntGen)
      , (10, scientific <$> pure 0
                        <*> bigIntGen)
      ]

    shrink s = zipWith scientific (QC.shrink $ Scientific.coefficient s)
                                  (QC.shrink $ Scientific.base10Exponent s)

nonNegativeScientificGen :: QC.Gen Scientific
nonNegativeScientificGen =
    scientific <$> (QC.getNonNegative <$> QC.arbitrary)
               <*> intGen

bigIntGen :: QC.Gen Int
bigIntGen = QC.sized $ \size -> QC.resize (size * 1000) intGen

intGen :: QC.Gen Int
#if MIN_VERSION_QuickCheck(2,7,0)
intGen = QC.arbitrary
#else
intGen = QC.sized $ \n -> QC.choose (-n, n)
#endif
