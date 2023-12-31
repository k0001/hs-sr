{-# LANGUAGE AllowAmbiguousTypes #-}

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
   , roundFromRational
   , fromFixed
   , divEither
   , div
   , recipEither
   , recip

    -- * Elimination
    -- $elimination
   , toScientific
   , roundToInteger
   , roundToFixed

    -- * Round
   , KR.Round (..)

    -- * Errors
   , Err (..)
   ) where

import Control.Applicative
import Control.DeepSeq (NFData (..))
import Control.Exception (Exception, displayException)
import Control.Monad
import Data.Aeson qualified as Ae
import Data.Aeson.Types qualified as Ae
import Data.Attoparsec.Text qualified as AT
import Data.Bifunctor
import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.SLEB128 qualified as SLEB128
import Data.Fixed
import Data.Hashable
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import GHC.Real (Ratio (..), divZeroError, (%))
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import KindRational qualified as KR
import Math.NumberTheory.Logarithms (integerLog10)
import Text.Read (readPrec)
import Prelude hiding (div, fromRational, recip)
import Prelude qualified as P

--------------------------------------------------------------------------------

-- $elimination
--
-- @
-- 'getField' \@\"s\" :: 'SR' -> 'Scientific'  /-- i.e., __sr.s__/
-- 'toScientific' :: 'SR' -> 'Scientific'
-- 'getField' \@\"r\" :: 'SR' -> 'Rational'    /-- i.e., __sr.r__/
-- 'toRational' :: 'SR' -> 'Rational'
-- 'roundToInteger' :: 'KR.Round' -> 'SR' -> ('Integer', 'SR')
-- 'roundToFixed' :: 'HasResolution' r => 'KR.Round' -> 'SR' -> ('Fixed' r, 'SR')
-- 'realToFrac' :: 'Fractional' a => 'SR' -> a
-- @

--------------------------------------------------------------------------------

data Err
   = Err_DenominatorIsZero
   | Err_DenominatorTooLarge
   | Err_DivisionByZero
   | Err_NotTerminating
   | Err_NumeratorTooLarge
   | Err_ScientificTooLarge
   | Err_ScientificTooSmall
   deriving stock (Eq, Ord, Show)

instance Exception Err where
   displayException = \case
      Err_DenominatorIsZero -> "Denominator is zero"
      Err_DenominatorTooLarge -> "Denominator too large"
      Err_DivisionByZero -> "Division by zero"
      Err_NotTerminating -> "Rational not terminating"
      Err_NumeratorTooLarge -> "Numerator too large"
      Err_ScientificTooLarge -> "Scientific too large"
      Err_ScientificTooSmall -> "Scientific too small"

--------------------------------------------------------------------------------

-- | A wrapper around a number that can be represented as both a 'Scientific'
-- and a 'Rational', implying that the number is not too big or small so as to
-- exhaust resources when being treated as 'Rational', and that it can be
-- represented using 'Scientific' notation because it is terminating.
-- That is, it doesn't lead to repeating decimals.
data SR = SR Scientific Rational

-- | May fail with 'Err_ScientiricTooSmall' or 'Err_ScientificTooLarge'.
--
-- @
-- forall (sr :: 'SR').
--    'fromScientificEither' ('toScientific' sr)  ==  'Right' sr
-- @
fromScientificEither :: Scientific -> Either Err SR
fromScientificEither = \a -> case S.toBoundedRealFloat a of
   Right (_ :: Double) -> Right $ SR a (toRational a)
   Left b
      | b == 0 -> Left Err_ScientificTooSmall
      | otherwise -> Left Err_ScientificTooLarge
{-# INLINEABLE fromScientificEither #-}

-- | Like 'fromScientificEither', but fails with 'fail'.
--
-- @
-- forall (sr :: 'SR').
--    'fromScientific' ('toScientific' sr)  ==  'pure' sr
-- @
fromScientific :: (MonadFail m) => Scientific -> m SR
fromScientific =
   either (fail . displayException) pure . fromScientificEither
{-# INLINE fromScientific #-}

-- | Like 'fromScientificEither', but fails with 'error'.
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromScientific' ('toScientific' sr)  ==  sr
-- @
unsafeFromScientific :: (HasCallStack) => Scientific -> SR
unsafeFromScientific =
   either (error . displayException) id . fromScientificEither
{-# INLINE unsafeFromScientific #-}

-- | May fail with 'Err_NotTerminating' or 'Err_DenominatorIsZero'.
--
-- @
-- forall (sr :: 'SR').
--    'fromRationalEither' ('toRational' sr)  ==  'Right' sr
-- @
fromRationalEither :: Rational -> Either Err SR
fromRationalEither = \a@(_ :% d) ->
   if d /= 0
      then
         if KR.isTerminating a
            then Right $ unsafestFromRational a
            else Left Err_NotTerminating
      else Left Err_DenominatorIsZero
{-# INLINEABLE fromRationalEither #-}

-- | Internal.
unsafestFromRational :: Rational -> SR
unsafestFromRational r = SR (S.unsafeFromRational r) r
{-# INLINEABLE unsafestFromRational #-}

-- | Like 'fromRationalEither', but fails with 'fail'.
--
-- @
-- forall (sr :: 'SR').
--    'fromRational' ('toRational' sr)  ==  'pure' sr
-- @
fromRational :: (MonadFail m) => Rational -> m SR
fromRational =
   either (fail . displayException) pure . fromRationalEither
{-# INLINE fromRational #-}

-- | Like 'fromRationalEither', but fails with 'error'.
--
-- @
-- forall (sr :: 'SR').
--    'unsafeFromRational' ('toRational' sr)  ==  sr
-- @
unsafeFromRational :: (HasCallStack) => Rational -> SR
unsafeFromRational =
   either (error . displayException) id . fromRationalEither
{-# INLINE unsafeFromRational #-}

-- | 'KR.Round' a 'Rational' into a 'SR'. Returns the 'Rational' reminder, too.
--
-- @
-- forall (rnd :: 'KR.Round') r (rat :: 'Rational').
--    ('HasResolution' r) =>
--        case 'roundFromRational' @r rnd rat
--           (sr, 0) -> 'fromRational' rat  ==  'Just' sr
--           _       -> 'fromRational' rat  ==  'Nothing'
-- @
--
-- @
-- forall (rnd :: 'KR.Round') r (rat :: 'Rational').
--    ('HasResolution' r) =>
--        case 'roundFromRational' @r rnd rat of
--           (sr, rest) -> 'toRational' sr + rest == rat
-- @
roundFromRational
   :: forall r. (HasResolution r) => KR.Round -> Rational -> (SR, Rational)
roundFromRational rnd = \a ->
   case roundRealFracToFixed @r rnd a of
      (f, r) -> (fromFixed f, r)
{-# INLINE roundFromRational #-}

-- | 'realToFrac', specialized to 'Fixed'. Probably a bit faster.
--
-- @
-- forall (rnd :: 'KR.Round') r (f :: 'Fixed' r).
--    ('HasResolution' r) =>
--        'roundToFixed' rnd ('fromFixed' f)  ==  (f, 0)
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
-- remainder, too.
--
-- @
-- forall (rnd :: 'KR.Round') r (f :: 'Fixed' r).
--    ('HasResolution' r) =>
--        'roundToFixed' rnd ('fromFixed' f)  ==  (f, 0)
-- @
--
-- @
-- forall (rnd :: 'KR.Round') r (sr :: 'SR').
--    ('HasResolution' r) =>
--        case 'roundToFixed' @r rnd sr of
--           (f, rest) -> 'fromFixed' f + rest == sr
-- @
roundToFixed :: (HasResolution r) => KR.Round -> SR -> (Fixed r, SR)
roundToFixed rnd = roundRealFracToFixed rnd
{-# INLINE roundToFixed #-}

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
-- remainder, too.
--
-- @
-- forall (rnd :: 'KR.Round') (i :: 'Integer').
--    'roundToInteger' rnd ('fromInteger' i)  ==  (i, 0)
-- @
--
-- @
-- forall (rnd :: 'KR.Round') (sr :: 'SR').
--    case 'roundToInteger' rnd sr of
--       (i, rest) -> 'fromInteger' i + rest == sr
-- @
roundToInteger :: KR.Round -> SR -> (Integer, SR)
roundToInteger rnd = \a ->
   case KR.divRem rnd a.r of
      (i, rest) -> (i, unsafeFromRational rest)
{-# INLINE roundToInteger #-}

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
   a == b = a.r == b.r
   {-# INLINE (==) #-}

instance Ord SR where
   compare a b = compare a.r b.r
   {-# INLINE compare #-}

instance Num SR where
   SR _ !ar + SR _ !br = unsafestFromRational (ar + br)
   {-# INLINE (+) #-}
   SR _ !ar - SR _ !br = unsafestFromRational (ar - br)
   {-# INLINE (-) #-}
   SR _ !ar * SR _ !br = unsafestFromRational (ar * br)
   {-# INLINE (*) #-}
   negate = \(SR _ !ar) -> unsafestFromRational (negate ar)
   {-# INLINE negate #-}
   abs = \(SR _ !ar) -> unsafestFromRational (abs ar)
   {-# INLINE abs #-}
   signum = \(SR _ !ar) -> unsafestFromRational (signum ar)
   {-# INLINE signum #-}
   fromInteger = \i -> unsafestFromRational (fromInteger i)
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
divEither :: SR -> SR -> Either Err SR
divEither a b =
   if b.r /= 0
      then fromRationalEither (a.r / b.r)
      else Left Err_DivisionByZero
{-# INLINE divEither #-}

-- | Safe version of "Prelude".'/'. Fails with 'fail' if divisor is 0.
div :: (MonadFail m) => SR -> SR -> m SR
div a b = either (fail . displayException) pure (divEither a b)
{-# INLINE div #-}

-- | Safe version of "Prelude".'recip'. Fails with 'Left' if 0, or if
-- reciprocal would be non-terminating.
recipEither :: SR -> Either Err SR
recipEither = \a ->
   if a.r /= 0
      then fromRationalEither (P.recip a.r)
      else Left Err_DivisionByZero
{-# INLINE recipEither #-}

-- | Safe version of "Prelude".'recip'. Fails with 'fail' if 0, or if
-- reciprocal would be non-terminating.
recip :: (MonadFail m) => SR -> m SR
recip = either (fail . displayException) pure . recipEither
{-# INLINE recip #-}

-- | See 'roundToInteger' and 'roundToFixed' for more general rounding tools.
instance RealFrac SR where
   properFraction = \(SR _ !ar) ->
      -- `ar` is known to be terminating, so `br` is too.
      let (i, br) = properFraction ar
      in  (i, unsafestFromRational br)
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

-- | Can decode a 'Ae.Number' or a 'Ae.String' containing
-- a 'Data.Attoparsec.scientific'.
instance Ae.FromJSON SR where
   parseJSON = \v -> Ae.prependFailure "SR: " do
      case v of
         Ae.Number s -> fromScientific s
         Ae.String t -> case AT.parseOnly atp t of
            Right (Right x) -> pure x
            Right (Left e) -> fail e
            _ -> fail msg
         _ -> fail msg
     where
      msg :: String
      msg =
         "Expected number in scientific notation, \
         \text containing a number in scientific notation, \
         \or text containing `[+-]numerator/denominator`"
      atp :: AT.Parser (Either String SR)
      atp =
         mplus
            (atpScientific <* AT.endOfInput)
            (atpRational <* AT.endOfInput)
      atpScientific :: AT.Parser (Either String SR)
      atpScientific = do
         s <- AT.scientific
         pure $ case fromScientificEither s of
            Right x -> Right x
            Left e -> Left ("Scientific: " <> displayException e)
      atpRational :: AT.Parser (Either String SR)
      atpRational = do
         let largeInteger = 10 ^ (100_000 :: Int) :: Integer
         n :: Integer <- AT.signed AT.decimal
         _ <- AT.char '/'
         d :: Integer <- toInteger @Integer <$> AT.decimal
         pure $
            first (mappend "Rational: " . displayException) $
               if
                  | d == 0 -> Left Err_DenominatorIsZero
                  | n == 0 -> Right 0
                  | abs n > largeInteger -> Left Err_NumeratorTooLarge
                  | d > largeInteger -> Left Err_DenominatorTooLarge
                  | otherwise -> fromRationalEither (n % d)

-- | Very compact representation using 'Data.Binary.ULEB128' and
-- 'Data.Binary.SLEB128' codecs. @0@ is represented with a single
-- byte. Other numbers take at least two bytes.
--
-- Additional compatible encoders:
-- "Data.Binary.SLEB128".'SLEB128.putScientific'.
--
-- Additional compatible decoders:
-- "Data.Binary.SLEB128".'SLEB128.getScientific'.
instance Bin.Binary SR where
   put = SLEB128.putScientific . toScientific
   {-# NOINLINE put #-}
   get = Bin.label "SR" $ fromScientific =<< SLEB128.getScientific 1000
   {-# NOINLINE get #-}
