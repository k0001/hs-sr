-- | This module exports the 'SR' type and tools for working with it.
--
-- Intended import usage:
--
-- @
-- import "SR" ('SR')
-- import "SR" qualified
-- @
module SR
   ( SR

    -- * Introduction
   , fromScientificEither
   , fromScientific
   , unsafeFromScientific
   , fromRationalEither
   , fromRational
   , unsafeFromRational
   , fromFixed
   , divEither
   , div
   , recipEither
   , recip

    -- * Elimination
    -- $elimination
   , toScientific

    -- ** Rounding
   , toInteger
   , toFixed
   , KR.Round (..)

    -- ** Extras
   , binaryPutScientific
   , binaryGetScientific
   ) where

import Control.DeepSeq (NFData (..))
import Data.Aeson qualified as Ae
import Data.Aeson.Types qualified as Ae
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.SLEB128 qualified as SLEB128
import Data.Binary.ULEB128 qualified as ULEB128
import Data.Bits
import Data.Fixed
import Data.Hashable
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import GHC.Real (divZeroError)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import KindRational qualified as KR
import Math.NumberTheory.Logarithms (integerLog10)
import Numeric.Natural (Natural)
import Text.Read (readPrec)
import Prelude hiding (div, fromRational, recip, toInteger)
import Prelude qualified as P

--------------------------------------------------------------------------------

-- $elimination
--
-- @
-- 'getField' \@\"s\" :: 'SR' -> 'Scientific'  /-- i.e., __sr.s__/
-- 'toScientific'  :: 'SR' -> 'Scientific'
-- 'getField' \@\"r\" :: 'SR' -> 'Rational'    /-- i.e., __sr.r__/
-- 'toRational'    :: 'SR' -> 'Rational'
-- 'toInteger'     :: 'KR.Round' -> 'SR' -> ('Integer', 'SR')
-- 'toFixed'       :: 'HasResolution' r => 'KR.Round' -> 'SR' -> ('Fixed' r, 'SR')
-- 'realToFrac'    :: 'Fractional' a => 'SR' -> a
-- @

--------------------------------------------------------------------------------

-- | A wrapper around a number that can be represented as both a 'Scientific'
-- and a 'Rational', implying that the number is not too big or small so as to
-- exhaust resources when being treated as 'Rational', and that it can be
-- represented using 'Scientific' notation because it is terminating.
-- That is, it doesn't lead to repeating decimals.
data SR = SR Scientific Rational

-- | Fails with 'Left' if 'Scientific' doesn't safely fit in 'Rational'.
--
-- @
-- forall (sr :: 'SR').
--    'fromScientificEither' ('toScientific' sr)  ==  'Right' sr
-- @
fromScientificEither :: Scientific -> Either String SR
fromScientificEither = \a -> case S.toBoundedRealFloat a of
   Right (_ :: Double) -> Right (SR a (toRational a))
   Left b
      | b == 0 -> Left "Scientific too small for Rational"
      | otherwise -> Left "Scientific too large for Rational"
{-# INLINEABLE fromScientificEither #-}

-- | Fails with 'fail' if 'Scientific' doesn't safely fit in 'Rational'.
--
-- @
-- forall (sr :: 'SR').
--    'fromScientific' ('toScientific' sr)  ==  'pure' sr
-- @
fromScientific :: (MonadFail m) => Scientific -> m SR
fromScientific = either fail pure . fromScientificEither
{-# INLINE fromScientific #-}

-- | Fails with 'error' if 'Scientific' doesn't safely fit in 'Rational'.
-- Prefer to use "SR".'fromScientific'.
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromScientific' ('toScientific' sr)  ==  sr
-- @
unsafeFromScientific :: (HasCallStack) => Scientific -> SR
unsafeFromScientific = either error id . fromScientificEither
{-# INLINE unsafeFromScientific #-}

-- | Fails with 'Left' if 'Rational' is not terminating.
--
-- @
-- forall (sr :: 'SR').
--    'fromRationalEither' ('toRational' sr)  ==  'Right' sr
-- @
fromRationalEither :: Rational -> Either String SR
fromRationalEither = \a ->
   if KR.isTerminating a
      then Right (SR (S.unsafeFromRational a) a)
      else Left "Non-terminating Rational"
{-# INLINEABLE fromRationalEither #-}

-- | Fails with 'fail' if 'Rational' is not terminating.
--
-- @
-- forall (sr :: 'SR').
--    'fromRational' ('toRational' sr)  ==  'pure' sr
-- @
fromRational :: (MonadFail m) => Rational -> m SR
fromRational = either fail pure . fromRationalEither
{-# INLINE fromRational #-}

-- | Fails with 'error' if the 'Rational' is not terminating. Prefer
-- to use "SR".'fromRational'.
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromRational' ('toRational' sr)  ==  sr
-- @
unsafeFromRational :: (HasCallStack) => Rational -> SR
unsafeFromRational = either error id . fromRationalEither
{-# INLINE unsafeFromRational #-}

-- | 'realToFrac', specialized to 'Fixed'. Probably a bit faster.
--
-- @
-- forall (rnd :: 'KR.Round') r (f :: 'Fixed' r).
--    ('HasResolution' r) =>
--        'toFixed' rnd ('fromFixed' f)  ==  (f, 0)
-- @
--
-- @
-- forall (rnd :: 'KR.Round') r (f :: 'Fixed' r).
--    ('HasResolution' r) =>
--        'fromFixed' f  ==  'realToFrac' f
-- @
fromFixed :: (HasResolution r) => Fixed r -> SR
fromFixed = \a -> SR (fixedToScientific a) (toRational a)
{-# INLINE fromFixed #-}

fixedToScientific :: forall r. (HasResolution r) => Fixed r -> Scientific
fixedToScientific = \(MkFixed i) -> S.scientific i e
  where
   e :: Int
   e = negate $ integerLog10 $ resolution $ Proxy @r
{-# INLINE fixedToScientific #-}

-- | 'KR.Round' a 'SR' so that it fits into the 'Fixed'. Returns the
-- remainder.
--
-- @
-- forall (rnd :: 'KR.Round') r (f :: 'Fixed' r).
--    ('HasResolution' r) =>
--        'toFixed' rnd ('fromFixed' f)  ==  (f, 0)
-- @
--
-- @
-- forall (rnd :: 'KR.Round') r (sr :: 'SR').
--    ('HasResolution' r) =>
--        case 'toFixed' @r rnd sr of
--           (f, rest) -> 'fromFixed' f + rest == sr
-- @
toFixed :: (HasResolution r) => KR.Round -> SR -> (Fixed r, SR)
toFixed rnd = roundRealFracToFixed rnd
{-# INLINE toFixed #-}

roundRealFracToFixed
   :: forall r a b
    . (HasResolution r, Real a, Fractional b)
   => KR.Round
   -> a
   -> (Fixed r, b)
roundRealFracToFixed rnd = \a ->
   case KR.divRem rnd (toRational a * res) of
      (d, rest) -> (MkFixed d, P.fromRational (rest / res))
  where
   res :: Rational
   res = toRational $ resolution $ Proxy @r
{-# INLINE roundRealFracToFixed #-}

-- | 'KR.Round' a 'SR' so that it fits into an 'Integer'. Returns the
-- remainder.
--
-- @
-- forall (rnd :: 'KR.Round') (i :: 'Integer').
--    'toInteger' rnd ('fromInteger' i)  ==  (i, 0)
-- @
--
-- @
-- forall (rnd :: 'KR.Round') (sr :: 'SR').
--    case 'toInteger' rnd sr of
--       (i, rest) -> 'fromInteger' i + rest == sr
-- @
toInteger :: KR.Round -> SR -> (Integer, SR)
toInteger rnd = \a ->
   case KR.divRem rnd a.r of
      (i, rest) -> (i, unsafeFromRational rest)
{-# INLINE toInteger #-}

-- | @
-- forall (sr :: 'SR').
--    'toScientific' sr  ==  sr.s  ==  'realToFrac' sr
-- @
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromScientific' ('toScientific' sr)  ==  sr
-- @
toScientific :: SR -> Scientific
toScientific = \(SR x _) -> x
{-# INLINE toScientific #-}

-- | @
-- forall (sr :: 'SR').
--    'toScientific' sr  ==  sr.s  ==  'realToFrac' sr
-- @
instance HasField "s" SR Scientific where
   getField = toScientific
   {-# INLINE getField #-}

{-# RULES
"realToFrac/SR->Scientific" realToFrac = toScientific
"realToFrac/SR->Rational" realToFrac = toRational @SR
"realToFrac/SR->SR" realToFrac = id @SR
   #-}

-- | @
-- forall (sr :: 'SR').
--    'toRational' sr  ==  sr.s  ==  'realToFrac' sr
-- @
instance HasField "r" SR Rational where
   getField = toRational
   {-# INLINE getField #-}

-- | @
-- forall (sr :: 'SR').
--    'read' ('show' sr)  ==  sr
-- @
--
-- @
-- forall (sr :: 'SR').
--    'show' sr  ==  'show' sr.s
-- @
instance Show SR where
   showsPrec n = showsPrec n . toScientific

-- | @'read' ('show' sr)  ==  sr@
instance Read SR where
   readPrec = fromScientific =<< readPrec

instance Eq SR where
   a == b = a.s == b.s
   {-# INLINE (==) #-}

instance Ord SR where
   compare a b = compare a.s b.s
   {-# INLINE compare #-}

instance Num SR where
   SR !as !ar + SR !bs !br = SR (as + bs) (ar + br)
   {-# INLINE (+) #-}
   SR !as !ar - SR !bs !br = SR (as - bs) (ar - br)
   {-# INLINE (-) #-}
   SR !as !ar * SR !bs !br = SR (as * bs) (ar * br)
   {-# INLINE (*) #-}
   negate = \(SR !as !ar) -> SR (negate as) (negate ar)
   {-# INLINE negate #-}
   abs = \(SR !as !ar) -> SR (abs as) (abs ar)
   {-# INLINE abs #-}
   signum = \(SR !as !ar) -> SR (signum as) (signum ar)
   {-# INLINE signum #-}
   fromInteger = \i -> SR (fromInteger i) (fromInteger i)
   {-# INLINE fromInteger #-}

-- | @
-- forall (sr :: 'SR').
--    'toRational' sr  ==  sr.s  ==  'realToFrac' sr
-- @
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromRational' ('toRational' sr)  ==  sr
-- @
instance Real SR where
   toRational = \(SR _ x) -> x
   {-# INLINE toRational #-}

-- | __WARNING__ 'Prelude.fromRational', '/' and 'Prelude.recip' may fail with
-- 'error', use "SR".'fromRational', "SR".'div' and "SR".'recip' instead.
instance Fractional SR where
   a / b =
      if b.r /= 0
         then unsafeFromRational (a.r / b.r)
         else divZeroError
   {-# INLINE (/) #-}
   fromRational = unsafeFromRational
   {-# INLINE fromRational #-}

-- | Safe version of "Prelude".'/'. Fails with 'Left' if divisor is 0.
divEither :: SR -> SR -> Either String SR
divEither a b =
   if b.r /= 0
      then fromRationalEither (a.r / b.r)
      else Left "Division by zero"
{-# INLINE divEither #-}

-- | Safe version of "Prelude".'/'. Fails with 'fail' if divisor is 0.
div :: (MonadFail m) => SR -> SR -> m SR
div a b = either fail pure (divEither a b)
{-# INLINE div #-}

-- | Safe version of "Prelude".'recip'. Fails with 'Left' if 0, or if
-- reciprocal would be non-terminating.
recipEither :: SR -> Either String SR
recipEither = \a ->
   if a.r /= 0
      then fromRationalEither (P.recip a.r)
      else Left "Division by zero"
{-# INLINE recipEither #-}

-- | Safe version of "Prelude".'recip'. Fails with 'fail' if 0, or if
-- reciprocal would be non-terminating.
recip :: (MonadFail m) => SR -> m SR
recip = either fail pure . recipEither
{-# INLINE recip #-}

-- | See 'toInteger' and 'toFixed' for more general rounding tools.
instance RealFrac SR where
   properFraction = \a ->
      let (i, br) = properFraction $! a.r
      in  -- `ar` is known to be terminating, so `br` is too.
          (i, SR (S.unsafeFromRational br) br)
   {-# INLINE properFraction #-}

instance NFData SR where
   rnf (SR ar as) = rnf ar `seq` rnf as `seq` ()
   {-# INLINE rnf #-}

instance Hashable SR where
   hashWithSalt salt = hashWithSalt salt . toScientific
   {-# INLINE hashWithSalt #-}

-- | @
-- 'Ae.toJSON'  ==  'Ae.toJSON' . 'toScientific'
-- @
instance Ae.ToJSON SR where
   toJSON = Ae.toJSON . toScientific
   {-# INLINE toJSON #-}

instance Ae.FromJSON SR where
   parseJSON v = Ae.prependFailure "SR: " do
      fromScientific =<< Ae.parseJSON v

-- | Very compact representation using 'Data.Binary.ULEB128' and
-- 'Data.Binary.SLEB128' codecs. @0@ is represented with a single
-- byte. Other numbers take at least two bytes.
--
-- Additional compatible encoders: 'binaryPutScientific'.
--
-- Additional compatible decoders: 'binaryGetScientific'.
instance Bin.Binary SR where
   put = binaryPutScientific . toScientific
   {-# NOINLINE put #-}
   get = Bin.label "SR" $ fromScientific =<< binaryGetScientific
   {-# NOINLINE get #-}

-- | Same compact 'Bin.Binary' encoder as the one used by 'SR'.
--
-- Compatible decoders: 'binaryGetScientific' and @'Bin.get' \@'SR'@.
binaryPutScientific :: S.Scientific -> Bin.Put
binaryPutScientific = \a ->
   if S.coefficient a /= 0
      then do
         -- The exponent is usually smaller than the coefficient, so we store
         -- the coefficient sing bit alongside the exponent to increase the
         -- chances that the coefficient fits in one less byte.
         let b = S.normalize a
             c = fromInteger $ abs $ S.coefficient b :: Natural
             e0 = P.toInteger (S.base10Exponent b :: Int) `unsafeShiftL` 1
             e1 = if b > 0 then e0 else setBit e0 0 :: Integer
         ULEB128.putNatural c
         SLEB128.putInteger e1
      else Bin.putWord8 0
{-# INLINABLE binaryPutScientific #-}

-- | Same compact 'Bin.Binary' decoder as the one used by 'SR', but without
-- the additional checks introduced by 'SR', so that it can be used with
-- standalone 'Scientific's.
--
-- Compatible encoders: 'binaryPutScientific' and @'Bin.put' \@'SR'@.
binaryGetScientific :: Bin.Get Scientific
binaryGetScientific = do
   c <- ULEB128.getInteger 200
   if c /= 0
      then do
         e1 <- SLEB128.getInteger 10 -- Valid e1 can't be longer than 10.
         case toIntegralSized (e1 `unsafeShiftR` 1) of
            Just (e0 :: Int)
               | testBit e1 0 -> pure $ S.scientific (negate c) e0
               | otherwise -> pure $ S.scientific c e0
            Nothing -> fail "Exponent too large"
      else pure 0
{-# INLINABLE binaryGetScientific #-}
