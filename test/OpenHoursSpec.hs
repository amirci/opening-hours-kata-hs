module OpenHoursSpec (main, spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Modifiers

import Test.Hspec

import OpenHours
import Data.Dates

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  context "When shop is open Mon, Wed, Fri from 8 to 4" $ do
    let hours = schedule [Monday, Wednesday, Friday] "08:00" "16:00"
    let wed = DateTime 2016 5 11 12 22 11
    let wedLate = DateTime 2016 5 11 18 22 11
    let wedEarly = DateTime 2016 5 11 6 22 11
    let thu = DateTime 2016 5 12 12 22 11
    let fridayMorning = DateTime 2016 5 13 8 0 0

    it "is open on Wed at 11:22:11" $ do
      OpenHours.isOpenOn wed hours `shouldBe` True

    it "is not open on Wed late" $ do
      OpenHours.isOpenOn wedLate hours `shouldBe` False

    it "is not open on Wed too early" $ do
      OpenHours.isOpenOn wedEarly hours `shouldBe` False

    it "is not open on Thu at 12:22:11" $ do
      OpenHours.isOpenOn thu hours `shouldBe` False

    it "returns next day friday morning" $ do
      OpenHours.nextOpeningDate thu hours `shouldBe` fridayMorning

    it "returns next day friday morning" $ do
      OpenHours.nextOpeningDate wed hours `shouldBe` fridayMorning
