{-# OPTIONS_HADDOCK hide #-}

module SR.Test
   ( hgen_Fixed_Default
   , hgen_Rational
   , hgen_Rational_Default
   , hgen_SR
   , hgen_SR_Default
   , hgen_Scientific
   , hgen_Scientific_Default
   , hprop_SR_Aeson_roundtrip
   , hprop_SR_Binary_roundtrip
   , hprop_SR_f
   , hprop_SR_i
   , hprop_SR_ops1
   , hprop_SR_ops2
   , hprop_SR_r
   , hprop_SR_s
   ) where

import Data.Aeson qualified as Ae
import Data.Binary as Bin
import Data.Fixed
import Data.Foldable
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import GHC.Real
import Hedgehog ((/==), (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR
import SR
import Text.Read
import Prelude as P

--------------------------------------------------------------------------------

hgen_Scientific_Default :: (H.MonadGen m) => m Scientific
hgen_Scientific_Default =
   hgen_Scientific
      (HR.linearFrom 0 (-2 ^ (99 :: Int)) (2 ^ (99 :: Int) - 1))
      (HR.linearFrom 0 (-33) 33)

hgen_Scientific
   :: (H.MonadGen m)
   => HR.Range Integer
   -- ^ 'S.coefficient'.
   -> HR.Range Int
   -- ^ 'S.base10Exponent'.
   -> m Scientific
hgen_Scientific rc re =
   S.scientific <$> HG.integral rc <*> HG.int re

hgen_Fixed_Default :: (HasResolution r, H.MonadGen m) => m (Fixed r)
hgen_Fixed_Default = P.fromRational <$> hgen_Rational_Default

hgen_Rational_Default :: (H.MonadGen m) => m Rational
hgen_Rational_Default = do
   let i = 10 ^ (99 :: Int) :: Integer
   hgen_Rational
      (HR.linearFrom 0 (negate i) (i - 1))
      (HR.linearFrom 1 1 (i - 1))

hgen_Rational
   :: (H.MonadGen m)
   => HR.Range Integer
   -- ^ Numerator
   -> HR.Range Integer
   -- ^ Denominator. 0 is discarded.
   -> m Rational
hgen_Rational ri rn =
   (%) <$> HG.integral ri <*> HG.filterT (/= 0) (HG.integral rn)

hgen_SR_Default :: (H.MonadGen m) => m SR
hgen_SR_Default =
   hgen_SR
      (HR.linearFrom 0 (-2 ^ (99 :: Int)) (2 ^ (99 :: Int) - 1))
      (HR.linearFrom 0 (-33) 33)

hgen_SR
   :: (H.MonadGen m)
   => HR.Range Integer
   -- ^ 'S.coefficient'.
   -> HR.Range Int
   -- ^ 'S.base10Exponent'.
   -> m SR
hgen_SR rc re = HG.mapMaybeT fromScientific (hgen_Scientific rc re)

hprop_SR_Binary_roundtrip :: H.Property
hprop_SR_Binary_roundtrip = H.property do
   trippingBin =<< H.forAll hgen_SR_Default

hprop_SR_Aeson_roundtrip :: H.Property
hprop_SR_Aeson_roundtrip = H.property do
   a <- H.forAll hgen_SR_Default
   H.tripping a Ae.encode Ae.decode
   H.tripping a (Ae.encode . show) Ae.decode
   H.tripping
      a
      ( \b ->
         Ae.encode $
            concat
               [ show (numerator b.r)
               , "/"
               , show (denominator b.r)
               ]
      )
      Ae.decode

hprop_SR_s :: H.Property
hprop_SR_s = H.property do
   a <- H.forAll hgen_Scientific_Default
   for_ (fromScientific @Maybe a) \b -> do
      b.s === a
      b.r === toRational a
      realToFrac b.r === a
      realToFrac b === a
      toScientific b === a
      fromScientificEither b.s === Right b
      fromRationalEither b.r === Right b
      fromScientific b.s === Just b
      SR.fromRational b.r === Just b
      P.fromRational b.r === a
      unsafeFromRational b.r === b
      unsafeFromScientific b.s === b

hprop_SR_r :: H.Property
hprop_SR_r = H.property do
   a@(n :% d) <- H.forAll hgen_Rational_Default
   rnd <- H.forAll HG.enumBounded
   case SR.fromRational a of
      Just b -> do
         b.r === a
         b.s === realToFrac a
         b.s === toScientific b
         toRational b.s === a
         toRational b === a
         realToFrac b === a
         fromScientificEither b.s === Right b
         fromRationalEither b.r === Right b
         fromScientific b.s === Just b
         SR.fromRational b.r === Just b
         P.fromRational b.r === a
         unsafeFromRational b.r === b
         unsafeFromScientific b.s === b
         roundFromRational @E0 rnd a === (b, 0)
         roundFromRational @E1 rnd a === (b, 0)
         roundFromRational @E9 rnd a === (b, 0)
         Just b === Ae.decode @SR (Ae.encode (show n <> "/" <> show d))
      Nothing -> do
         Nothing === Ae.decode @SR (Ae.encode (show n <> "/" <> show d))
         case roundFromRational @E0 rnd a of
            (sr, rest) -> do
               rest /== 0
               toRational sr + rest === a
         case roundFromRational @E1 rnd a of
            (sr, rest) -> do
               rest /== 0
               toRational sr + rest === a
         case roundFromRational @E9 rnd a of
            (sr, rest) -> do
               rest /== 0
               toRational sr + rest === a

hprop_SR_f :: H.Property
hprop_SR_f = H.property do
   rnd <- H.forAll HG.enumBounded
   do
      f <- H.forAll (hgen_Fixed_Default @E0)
      roundToFixed rnd (fromFixed f) === (f, 0)
   do
      f <- H.forAll (hgen_Fixed_Default @E1)
      roundToFixed rnd (fromFixed f) === (f, 0)
   do
      f <- H.forAll (hgen_Fixed_Default @E9)
      roundToFixed rnd (fromFixed f) === (f, 0)

hprop_SR_i :: H.Property
hprop_SR_i = H.property do
   rnd <- H.forAll HG.enumBounded
   i <-
      H.forAll $
         HG.integral $
            HR.linearFrom
               0
               (negate (10 ^ (99 :: Int)))
               (10 ^ (99 :: Int) - 1)
   roundToInteger rnd (fromInteger i) === (i, 0)

hprop_SR_ops1 :: H.Property
hprop_SR_ops1 = H.property do
   a <- H.forAll hgen_SR_Default

   a.r === toRational a.s
   a.r === toRational a
   a.s === realToFrac a.r
   a.s === toScientific a
   toRational a === a.r
   realToFrac a === a.r
   realToFrac a === a.s
   fromScientificEither a.s === Right a
   fromRationalEither a.r === Right a
   fromScientific a.s === Just a
   SR.fromRational a.r === Just a
   P.fromRational a.r === a
   unsafeFromScientific a.s === a
   unsafeFromRational a.r === a

   case properFraction a of
      (i, b) -> a === fromInteger i + b

   (negate a).s === negate a.s
   (negate a).r === negate a.r

   (abs a).s === abs a.s
   (abs a).r === abs a.r

   (signum a).s === signum a.s
   (signum a).r === signum a.r

   show a === show a.s

   H.tripping a show readEither

   for_ (SR.recip @Maybe a) \b -> do
      b === P.recip a
      b.s === P.recip a.s
      b.r === P.recip a.r

   rnd <- H.forAll HG.enumBounded
   case roundToFixed @E0 rnd a of
      (f, rest) -> a === fromFixed f + rest
   case roundToFixed @E1 rnd a of
      (f, rest) -> a === fromFixed f + rest
   case roundToFixed @E9 rnd a of
      (f, rest) -> a === fromFixed f + rest

hprop_SR_ops2 :: H.Property
hprop_SR_ops2 = H.property do
   a <- H.forAll hgen_SR_Default
   b <- H.forAll hgen_SR_Default

   (a + b).s === a.s + b.s
   (a + b).r === a.r + b.r

   (a * b).s === a.s * b.s
   (a * b).r === a.r * b.r

   (a == b) === (a.s == b.s)
   (a == b) === (a.r == b.r)

   compare a b === compare a.s b.s
   compare a b === compare a.r b.r

   for_ (SR.div @Maybe a b) \c -> do
      c === a / b
      c.s === a.s / b.s
      c.r === a.r / b.r

trippingBin
   :: forall a m
    . (Eq a, Show a, Bin.Binary a, H.MonadTest m)
   => a
   -> m ()
trippingBin a0 = H.tripping a0 Bin.encode \bl ->
   case Bin.decodeOrFail bl of
      Right ("", _, a1) -> Right a1
      Right _ -> Left "Unexpected leftovers"
      Left (_, _, e) -> Left e
