module TexasHoldEm.AcceptanceSpec where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TexasHoldEm.Game (calculateHands, determineWinners)
import TexasHoldEm.Formatter (format)
import TexasHoldEm.Parser (parse)

spec :: Spec Unit
spec =
  describe "Texas Hold’Em Championship show" do
    it "should knew what hands the players were holding and which hand won the round" do
      let input = """
        Kc 9s Ks Kd 9d 3c 6d
        9c Ah Ks Kd 9d 3c 6d
        Ac Qc Ks Kd 9d 3c
        9h 5s
        4d 2d Ks Kd 9d 3c 6d
        7s Ts Ks Kd 9d
      """

      let output = do
           cards <- parse input
           hands <- calculateHands cards
           let winners = determineWinners hands
           pure $ format hands winners

      let expectedOutput = """
        Kc 9s Ks Kd 9d 3c 6d Full House (winner)
        9c Ah Ks Kd 9d 3c 6d Two Pair
        Ac Qc Ks Kd 9d 3c
        9h 5s
        4d 2d Ks Kd 9d 3c 6d Flush
        7s Ts Ks Kd 9d
      """
      output `shouldEqual` (Right expectedOutput)
