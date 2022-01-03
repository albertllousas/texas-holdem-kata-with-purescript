module TexasHoldEm.FormatterSpec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TexasHoldEm.Game (Rank(..), Card(..), Suite(..), GameError(..), (*))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (fst, snd)
import TexasHoldEm.Formatter (formatRank, formatSuite, formatCard)
import Data.List.Types (List(..), (:))
import TexasHoldEm.Formatter (formatCards)
import Data.List (zip)
import TexasHoldEm.Formatter (formatHand)
import TexasHoldEm.Game (Hand(..))
import TexasHoldEm.Game (BestCombination(..))
import TexasHoldEm.Game (Ranking(..))
import TexasHoldEm.Formatter (format)

spec :: Spec Unit
spec =
  describe "format" do

      it "should format a Rank" do
        let ranks = Ace:Two:Three:Four:Five:Six:Seven:Eight:Nine:Ten:Jack:Queen:King:Nil
        let expectedStrings = "A":"2":"3":"4":"5":"6":"7":"8":"9":"T":"J":"Q":"K":Nil
        let expectations = zip ranks expectedStrings
        for_ expectations (\expectation -> formatRank (fst expectation) `shouldEqual` (snd expectation))

      it "should format a suite" do
        let suites = Clubs:Diamonds:Hearts:Spades:Nil
        let expectedStrings = "c":"d":"h":"s":Nil
        let expectations = zip suites expectedStrings
        for_ expectations (\expectation -> formatSuite (fst expectation) `shouldEqual` (snd expectation))

      it "should format a card " do
        formatCard (Card Ace Clubs) `shouldEqual` "Ac"

      it "should format a list of cards" do
        formatCards ((Card Ace Clubs):(Card Three Spades):Nil) `shouldEqual` "Ac 3s"

      it "should format a folded hand" do
        let hand = (Fold ((Card Ace Clubs):(Card Three Spades):Nil))
        let winners = Nil
        formatHand hand winners `shouldEqual` "Ac 3s"

      it "should format a non winner final hand" do
        let hand = FinalHand (BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let winners = Nil
        formatHand hand winners `shouldEqual` "Tc Qh Kd Jc 9c Straight"

      it "should format a winner final hand" do
        let hand = FinalHand (BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let winners = hand : Nil
        formatHand hand winners `shouldEqual` "Tc Qh Kd Jc 9c Straight (winner)"

      it "should format a list of hands" do
        let winner = FinalHand (BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let hand = FinalHand (BestCombination ThreeOfAKind ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let winners = winner : Nil
        format (winner : hand : Nil) winners `shouldEqual` "Tc Qh Kd Jc 9c Straight (winner)\nTc Th Td Jc 9c ThreeOfAKind"
