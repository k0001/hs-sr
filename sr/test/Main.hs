module Main (main) where

import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (..), testProperty)
import Test.Tasty.Runners qualified as Tasty

import SR.Test qualified as SR

--------------------------------------------------------------------------------

main :: IO ()
main =
   Tasty.defaultMainWithIngredients
      [ Tasty.consoleTestReporter
      , Tasty.listingTests
      ]
      $ Tasty.localOption (HedgehogTestLimit (Just 5000))
      $ Tasty.testGroup
         "SR"
         [ testProperty "Aeson_roundtrip" SR.hprop_SR_Aeson_roundtrip
         , testProperty "Binary_roundtrip" SR.hprop_SR_Binary_roundtrip
         , testProperty "f" SR.hprop_SR_f
         , testProperty "ops1" SR.hprop_SR_ops1
         , testProperty "ops2" SR.hprop_SR_ops2
         , testProperty "r" SR.hprop_SR_r
         , testProperty "s" SR.hprop_SR_s
         , testProperty "i" SR.hprop_SR_i
         ]
