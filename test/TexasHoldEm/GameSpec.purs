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
import TexasHoldEm.Game (BestCombination(..))
import TexasHoldEm.Game (Card(..))
import TexasHoldEm.Game (determineWinners)

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

      it "should rank a hand that contains two cards of one rank and three cards of three other ranks, as a One Pair" do
        let pair = (Nine*Clubs):(Nine*Diamonds):(Ten*Spades):(Two*Diamonds):(Five*Diamonds):Nil
        rankCards pair `shouldEqual` OnePair

      it "should rank a hand that does not fall into any other category, as a High Card" do
        let highCard = (Nine*Clubs):(Eight*Diamonds):(Ten*Spades):(Two*Diamonds):(Five*Diamonds):Nil
        rankCards highCard `shouldEqual` HighCard

    describe "calculate a hand given seven cards (two hole cards and five community cards)" do

      it "calculate the best hand when there are multiple combinations to rank" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):(Ten*Clubs):Nil
        let bestCombination = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Clubs):Nil
        calculateHand hand `shouldEqual` Right (FinalHand (BestCombination Straight bestCombination) hand)

      it "should fold when there are not enough community cards (less than five)" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):Nil
        calculateHand hand `shouldEqual` Right (Fold hand)

      it "should fail when there are not enough hole cards" do
        let hand = (Nine*Clubs):Nil
        calculateHand hand `shouldEqual` Left (HandWithNotEnoughCards hand)

      it "should fail when there are too many community cards" do
        let hand = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):(Ten*Clubs):(Ten*Clubs):Nil
        calculateHand hand `shouldEqual` Left (HandWithTooManyCards hand)

    describe "determine winners" do

      it "shouldn't be any winners when all the hands are Fold" do
        let hands = (Fold Nil):(Fold Nil):(Fold Nil):(Fold Nil):(Fold Nil):Nil
        determineWinners hands `shouldEqual` Nil

      it "should determine the winners when there are multiple final hands" do
        let firstStraight = FinalHand (BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) Nil
        let secondStraight = FinalHand (BestCombination Straight ((Ten*Hearts):(Queen*Diamonds):(King*Hearts):(Jack*Clubs):(Nine*Hearts):Nil)) Nil
        let threeOfAKind = FinalHand (BestCombination ThreeOfAKind ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)) Nil
        let twoPair = FinalHand (BestCombination TwoPair ((Ten*Clubs):(Ten*Hearts):(Two*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)) Nil
        let hands = firstStraight:secondStraight:threeOfAKind:twoPair:(Fold Nil):Nil
        determineWinners hands `shouldEqual` (firstStraight:secondStraight:Nil)

    describe "compare hands" do

      it "two folded hands should be equal" do
        compare (Fold ((Nine*Clubs):Nil)) (Fold ((Ace*Clubs):Nil)) `shouldEqual` EQ

      it "a final hand should be greater than a fold" do
        let allCards = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):(Ten*Spades):(Ten*Clubs):Nil
        let bestCombination = (Nine*Clubs):(Queen*Diamonds):(King*Spades):(Jack*Diamonds):(Ten*Diamonds):Nil
        let finalHand = FinalHand (BestCombination Straight bestCombination) allCards
        let fold = Fold allCards
        compare finalHand fold `shouldEqual` GT
        compare fold finalHand `shouldEqual` LT

      it "two final hands should compared using their best combinations" do
        let straight = FinalHand (BestCombination Straight Nil) Nil
        let fullHouse = FinalHand (BestCombination FullHouse Nil) Nil
        compare straight fullHouse `shouldEqual` LT

    describe "compare best combinations of a two different hands (ranking with their corresponding cards)" do

      it "should compare by rank when rankings are different" do
        let onePair = BestCombination OnePair Nil
        let twoPair = BestCombination TwoPair Nil
        compare onePair twoPair `shouldEqual` LT

      it "if two hands hold Royal Flush, the pot is divided" do
        let firstRoyalFlush = BestCombination RoyalFlush ((Ten*Clubs):(Queen*Clubs):(King*Clubs):(Jack*Clubs):(Ace*Clubs):Nil)
        let secondRoyalFlush = BestCombination RoyalFlush ((Ten*Hearts):(Queen*Hearts):(King*Hearts):(Jack*Hearts):(Ace*Hearts):Nil)
        compare firstRoyalFlush secondRoyalFlush `shouldEqual` EQ

      it "if two hands hold the same straight flush, the pot is divided" do
        let firstStraightFlush = BestCombination StraightFlush ((Ten*Clubs):(Queen*Clubs):(King*Clubs):(Jack*Clubs):(Nine*Clubs):Nil)
        let secondStraightFlush = BestCombination StraightFlush ((Ten*Hearts):(Queen*Hearts):(King*Hearts):(Jack*Hearts):(Nine*Hearts):Nil)
        compare firstStraightFlush secondStraightFlush `shouldEqual` EQ

      it "if two hands hold straight flush, then the higher ranking straight flush wins" do
        let firstStraightFlush = BestCombination StraightFlush ((Ten*Clubs):(Queen*Clubs):(King*Clubs):(Eight*Clubs):(Nine*Clubs):Nil)
        let secondStraightFlush = BestCombination StraightFlush ((Ten*Hearts):(Queen*Hearts):(King*Hearts):(Jack*Hearts):(Nine*Hearts):Nil)
        compare firstStraightFlush secondStraightFlush `shouldEqual` LT

      it "if two hands hold the same Four of a kind with the same kicker, the pot is divided" do
        let firstFourOfAKind = BestCombination FourOfAKind ((Nine*Clubs):(Five*Clubs):(Nine*Hearts):(Nine*Diamonds):(Nine*Spades):Nil)
        let secondFourOfAKind = BestCombination FourOfAKind ((Nine*Clubs):(Five*Clubs):(Nine*Hearts):(Nine*Diamonds):(Nine*Spades):Nil)
        compare firstFourOfAKind secondFourOfAKind `shouldEqual` EQ

      it "if two hands hold Four of a kind, the hand with the highest-ranking four of a kind wins" do
        let firstFourOfAKind = BestCombination FourOfAKind ((Nine*Clubs):(Five*Clubs):(Nine*Hearts):(Nine*Diamonds):(Nine*Spades):Nil)
        let secondFourOfAKind = BestCombination FourOfAKind ((Ten*Clubs):(Five*Clubs):(Ten*Hearts):(Ten*Diamonds):(Ten*Spades):Nil)
        compare firstFourOfAKind secondFourOfAKind `shouldEqual` LT

      it "if two hands hold the same Four of a kind, the hand with the highest-ranking kicker wins" do
        let firstFourOfAKind = BestCombination FourOfAKind ((Ten*Clubs):(Queen*Clubs):(Ten*Hearts):(Ten*Diamonds):(Ten*Spades):Nil)
        let secondFourOfAKind = BestCombination FourOfAKind ((Ten*Clubs):(Five*Clubs):(Ten*Hearts):(Ten*Diamonds):(Ten*Spades):Nil)
        compare firstFourOfAKind secondFourOfAKind `shouldEqual` GT

      it "if two hands hold the same Full House, the pot is divided" do
        let firstFourOfAKind = BestCombination FullHouse ((Ace*Clubs):(Nine*Clubs):(Ace*Hearts):(Ace*Diamonds):(Nine*Spades):Nil)
        let secondFourOfAKind = BestCombination FullHouse ((Ace*Clubs):(Nine*Clubs):(Ace*Hearts):(Ace*Diamonds):(Nine*Spades):Nil)
        compare firstFourOfAKind secondFourOfAKind `shouldEqual` EQ

      it "if two hands hold Full House, the hand with the highest three of a kind wins" do
        let firstFullHouse = BestCombination FullHouse ((Ace*Clubs):(Nine*Clubs):(Ace*Hearts):(Ace*Diamonds):(Nine*Spades):Nil)
        let secondFullHouse = BestCombination FullHouse ((King*Clubs):(Nine*Clubs):(King*Hearts):(King*Diamonds):(Nine*Spades):Nil)
        compare firstFullHouse secondFullHouse `shouldEqual` GT

      it "if two hands hold Full House with the same three of a kind, the hand with the highest pair wins" do
        let firstFullHouse = BestCombination FullHouse ((Ace*Clubs):(Nine*Clubs):(Ace*Hearts):(Ace*Diamonds):(Nine*Spades):Nil)
        let secondFullHouse = BestCombination FullHouse ((Ace*Clubs):(Two*Clubs):(Ace*Hearts):(Ace*Diamonds):(Two*Spades):Nil)
        compare firstFullHouse secondFullHouse `shouldEqual` GT

      it "if two hands hold the same Flush, the pot is divided" do
        let firstFlush = BestCombination Flush ((Ace*Clubs):(Nine*Clubs):(Two*Clubs):(Five*Clubs):(Seven*Clubs):Nil)
        let secondFlush = BestCombination Flush ((Ace*Hearts):(Nine*Hearts):(Five*Hearts):(Seven*Hearts):(Two*Hearts):Nil)
        compare firstFlush secondFlush `shouldEqual` EQ

      it "if two hands hold Flush, the hand with the highest-ranking flush wins" do
        let firstFlush = BestCombination Flush ((Ace*Clubs):(Nine*Clubs):(Two*Clubs):(Five*Clubs):(Seven*Clubs):Nil)
        let secondFlush = BestCombination Flush ((King*Hearts):(Nine*Hearts):(Five*Hearts):(Seven*Hearts):(Two*Hearts):Nil)
        compare firstFlush secondFlush `shouldEqual` GT

      it "if two hands hold the same straight, the pot is divided" do
         let firstStraight = BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
         let secondStraight = BestCombination Straight ((Ten*Hearts):(Queen*Diamonds):(King*Hearts):(Jack*Clubs):(Nine*Hearts):Nil)
         compare firstStraight secondStraight `shouldEqual` EQ

      it "if two hands hold straight flush, then the higher ranking straight flush wins" do
        let firstStraight = BestCombination Straight ((Ten*Clubs):(Queen*Hearts):(King*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let secondStraight = BestCombination Straight ((Ten*Hearts):(Queen*Diamonds):(King*Hearts):(Jack*Clubs):(Ace*Hearts):Nil)
        compare firstStraight secondStraight `shouldEqual` LT

      it "if two hands hold an identical three of a kind with the same kickers, the pot is divided" do
        let firstThreeOfAKind = BestCombination ThreeOfAKind ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let secondThreeOfAKind = BestCombination ThreeOfAKind ((Jack*Hearts):(Nine*Diamonds):(Ten*Hearts):(Ten*Clubs):(Ten*Hearts):Nil)
        compare firstThreeOfAKind secondThreeOfAKind `shouldEqual` EQ

      it "if two hands hold an identical three of a kind, then the hand with the 4th strongest card wins" do
        let firstThreeOfAKind = BestCombination ThreeOfAKind ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(Jack*Clubs):(Nine*Clubs):Nil)
        let secondThreeOfAKind = BestCombination ThreeOfAKind ((King*Hearts):(Five*Diamonds):(Ten*Hearts):(Ten*Clubs):(Ten*Hearts):Nil)
        compare firstThreeOfAKind secondThreeOfAKind `shouldEqual` GT

      it "if two hands hold an identical three of a kind and also the same 4th strongest card, then the strongest card 5th card will decide" do
        let firstThreeOfAKind = BestCombination ThreeOfAKind ((Ten*Clubs):(Ten*Hearts):(Ten*Diamonds):(King*Clubs):(Nine*Clubs):Nil)
        let secondThreeOfAKind = BestCombination ThreeOfAKind ((King*Hearts):(Five*Diamonds):(Ten*Hearts):(Ten*Clubs):(Ten*Hearts):Nil)
        compare firstThreeOfAKind secondThreeOfAKind `shouldEqual` GT

      it "if two hands hold two pairs each, the hand with the larger top pair will win" do
        let firstTwoPair = BestCombination TwoPair ((Ten*Clubs):(Ten*Hearts):(Two*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)
        let secondTwoPair = BestCombination TwoPair ((Nine*Hearts):(Nine*Diamonds):(Eight*Hearts):(Eight*Clubs):(Ten*Hearts):Nil)
        compare firstTwoPair secondTwoPair `shouldEqual` GT

      it "if two hands hold two pairs each with the same higher pair, then the pair with the highest second pair wins" do
        let firstTwoPair = BestCombination TwoPair ((Ten*Clubs):(Ten*Hearts):(Two*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)
        let secondTwoPair = BestCombination TwoPair ((Ten*Hearts):(Ten*Diamonds):(Eight*Hearts):(Eight*Clubs):(King*Hearts):Nil)
        compare firstTwoPair secondTwoPair `shouldEqual` LT

      it "if two hands hold two identical pairs each, the hand with the highest 5th card wins" do
        let firstTwoPair = BestCombination TwoPair ((Ten*Clubs):(Ten*Hearts):(Two*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)
        let secondTwoPair = BestCombination TwoPair ((Ten*Hearts):(Ten*Diamonds):(Two*Hearts):(Two*Clubs):(Ten*Hearts):Nil)
        compare firstTwoPair secondTwoPair `shouldEqual` LT

      it "if two hands hold two identical pairs each with the same 5th card, the pot is divided" do
        let firstTwoPair = BestCombination TwoPair ((Ten*Clubs):(Ten*Hearts):(Two*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)
        let secondTwoPair = BestCombination TwoPair ((Ten*Hearts):(Ten*Diamonds):(Two*Hearts):(Two*Clubs):(Nine*Hearts):Nil)
        compare firstTwoPair secondTwoPair `shouldEqual` EQ

      it "if two hands hold pairs each, the hand with the larger pair will win" do
        let firstPair = BestCombination OnePair ((Ten*Clubs):(Ten*Hearts):(Eight*Diamonds):(Two*Clubs):(Nine*Clubs):Nil)
        let secondPair = BestCombination OnePair ((Nine*Hearts):(Nine*Diamonds):(Eight*Hearts):(Ten*Clubs):(Queen*Hearts):Nil)
        compare firstPair secondPair `shouldEqual` GT

      it "if two hands hold two identical pairs each, the hand with the best kickers wins" do
        let firstPair = BestCombination OnePair ((Ten*Clubs):(Ten*Hearts):(Eight*Diamonds):(Two*Clubs):(Three*Clubs):Nil)
        let secondPair = BestCombination OnePair ((Nine*Hearts):(Nine*Diamonds):(Seven*Hearts):(Six*Clubs):(Five*Hearts):Nil)
        compare firstPair secondPair `shouldEqual` GT

      it "if two hands hold the same identical five cards that are not matching any ranking, the pot is Shared" do
        let firstHand = BestCombination HighCard ((Ten*Clubs):(Nine*Hearts):(Eight*Diamonds):(King*Clubs):(Three*Clubs):Nil)
        let secondHand = BestCombination HighCard ((Ten*Hearts):(Nine*Diamonds):(Eight*Hearts):(King*Clubs):(Three*Hearts):Nil)
        compare firstHand secondHand `shouldEqual` EQ

      it "if two hands hold five cards that are not matching any ranking, the hand with the first different highest card wins" do
        let firstHand = BestCombination HighCard ((Ten*Clubs):(Nine*Hearts):(Eight*Diamonds):(King*Clubs):(Two*Clubs):Nil)
        let secondHand = BestCombination HighCard ((Ten*Hearts):(Nine*Diamonds):(Eight*Hearts):(King*Clubs):(Three*Hearts):Nil)
        compare firstHand secondHand `shouldEqual` LT

