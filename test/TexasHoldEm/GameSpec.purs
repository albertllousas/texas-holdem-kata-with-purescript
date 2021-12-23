module TexasHoldEm.GameSpec where

import Prelude hiding ((*))

import Data.List ((:))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TexasHoldEm.Generic (kCombinations)
import Data.List as List
import Data.List.Types (List(..))
import TexasHoldEm.Game (Rank(..), Suite(..), (*), Ranking(..), rankCards)
import TexasHoldEm.Game (isFlush)
import TexasHoldEm.Game (isStraight)
import TexasHoldEm.Game (calculateHand)
import TexasHoldEm.Game (Hand(..))
import TexasHoldEm.Game (GameError(..))
import TexasHoldEm.Game (RankedCards(..))

spec ::Spec Unit
spec =
  describe "game" do

    describe "rank cards" do

      it "should rank a hand with an ace, king, queen, jack, ten, all in the same suit, as a Royal Flush" do
        let royalFlush = (Ace*Clubs):(Queen*Clubs):(King*Clubs):(Jack*Clubs):(Ten*Clubs):Nil
        rankCards royalFlush `shouldEqual` RoyalFlush

      it "should rank a hand that contains five cards of sequential rank, all of the same suit, as a Straight Flush" do
        let straightFlush = (Nine*Diamonds):(Queen*Diamonds):(King*Diamonds):(Jack*Diamonds):(Ten*Diamonds):Nil
        rankCards straightFlush `shouldEqual` StraightFlush

      it "should rank a hand that contains four cards of one rank as a Four of a Kind" do
        let fourOfAKind = (Ace*Diamonds):(Queen*Diamonds):(Ace*Clubs):(Ace*Spades):(Ace*Hearts):Nil
        rankCards fourOfAKind `shouldEqual` FourOfAKind

      it "should rank a hand that contains three cards of one rank and two cards of another rank, as a Full House" do
        let fullHouse = (Ace*Diamonds):(Two*Diamonds):(Two*Clubs):(Ace*Spades):(Ace*Hearts):Nil
        rankCards fullHouse `shouldEqual` FullHouse

      it "should rank a hand that contains five cards all of the same suit, not all of sequential rank, as a Fush" do
        let flush = (Ace*Diamonds):(Two*Diamonds):(Three*Diamonds):(King*Diamonds):(Ten*Diamonds):Nil
        rankCards flush `shouldEqual` Flush

      it "should rank a hand that contains five cards of sequential rank, not all of the same suit, as a Straight" do
         let straight = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):Nil
         rankCards straight `shouldEqual` Straight

      it "should rank a hand that contains three cards of one rank and two other cards, as a Three of a Kind" do
         let threeOfAKind = (Nine*Clubs):(Nine*Diamonds):(Nine*Spades):(Jack*Diamonds):(Ten*Diamonds):Nil
         rankCards threeOfAKind `shouldEqual` ThreeOfAKind

      it "should rank a hand with two cards of one rank, two cards of another rank and other card, as a Two Pair " do
        let twoPair = (Nine*Clubs):(Nine*Diamonds):(Ten*Spades):(Ten*Diamonds):(Five*Diamonds):Nil
        rankCards twoPair `shouldEqual` TwoPair

      it "should rank a hand that contains two cards of one rank and three cards of three other ranks, as a OnePair" do
        let pair = (Nine*Clubs):(Nine*Diamonds):(Ten*Spades):(Two*Diamonds):(Five*Diamonds):Nil
        rankCards pair `shouldEqual` OnePair

      it "should rank a hand that does not fall into any other category, as a High Card" do
        let highCard = (Nine*Clubs):(Eight*Diamonds):(Ten*Spades):(Two*Diamonds):(Five*Diamonds):Nil
        rankCards highCard `shouldEqual` HighCard

    describe "calculate a hand given seven cards, two hole cards and five community cards" do

      it "calculate the best hand when there are multiple combinations to rank" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):(Ten*Clubs):Nil
        let bestCombination = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):Nil
        calculateHand hand `shouldEqual` Right (BestHand (RankedCards Straight bestCombination) hand)

      it "should fold when there are not enough community cards (less than five)" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):Nil
        calculateHand hand `shouldEqual` Right (Fold hand)

      it "should fail when there are not enough hole cards" do
        let hand = (Nine*Clubs):Nil
        calculateHand hand `shouldEqual` Left (HandWithNotEnoughCards hand)

      it "should fail when there are too many community cards" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):(Ten*Clubs):(Ten*Clubs):Nil
        calculateHand hand `shouldEqual` Left (HandWithTooManyCards hand)





