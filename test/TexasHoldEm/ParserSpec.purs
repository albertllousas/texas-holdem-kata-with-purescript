module TexasHoldEm.ParserSpec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TexasHoldEm.Game (Rank(..), Card(..), Suite(..), GameError(..))
import Data.Either (Either(..))
import Data.Array ((:), zip)
import TexasHoldEm.Parser (parseCard, parseRank, parseSuite, parse, parseSingleLine, defaultParserConfig)
import Data.Foldable (for_)
import Data.List as List
import Data.Tuple (fst, snd)

spec :: Spec Unit
spec =
  describe "parse" do

    describe "parse rank" do

      it "should parse a rank from valid character" do
        let chars = 'A':'2':'3':'4':'5':'6':'7':'8':'9':'T':'J':'Q':'K':[]
        let expectedRanks = Ace:Two:Three:Four:Five:Six:Seven:Eight:Nine:Ten:Jack:Queen:King:[]
        let expectations = zip chars expectedRanks
        for_ expectations (\expectation -> parseRank (fst expectation) `shouldEqual` Right (snd expectation))

      it "should fail parsing a rank from an invalid character" do
        parseRank 'x' `shouldEqual` Left (InvalidRank "x")

    describe "parse suite" do

      it "should parse a suite from valid character" do
        let chars = 'c':'d':'h':'s':[]
        let expectedSuites = Clubs:Diamonds:Hearts:Spades:[]
        let expectations = zip chars expectedSuites
        for_ expectations (\expectation -> parseSuite (fst expectation) `shouldEqual` Right (snd expectation))

      it "should fail parsing a suite from an invalid character" do
        parseSuite 'x' `shouldEqual` Left (InvalidSuite "x")

    describe "parse card" do

      let parse = parseCard defaultParserConfig

      it "should parse a card from a valid string" do
        parse "Ac" `shouldEqual` Right (Card Ace Clubs)

      it "should fail parsing a card from an empty string" do
        parse "" `shouldEqual` Left (InvalidCard "")

      it "should fail parsing a card from an invalid string" do
        parse "invalid" `shouldEqual` Left (InvalidCard "invalid")

      it "should fail parsing a card from an invalid rank" do
        parse "ac" `shouldEqual` Left (InvalidRank "a")

      it "should fail parsing a card from an invalid suite" do
        parse "AC" `shouldEqual` Left (InvalidSuite "C")

    describe "parse a list of cards" do

      let parse = parseSingleLine defaultParserConfig

      it "should parse a list of cards from a valid string" do
        parse "Ac 3s" `shouldEqual` Right (List.fromFoldable ((Card Ace Clubs):(Card Three Spades):[]))

      it "should fail parsing a list of cards from an invalid string" do
        parse "Ac 3ss" `shouldEqual` Left (InvalidCard "3ss")

    describe "parse a matrix of cards" do

       it "should parse a matrix of cards from a valid multiline string" do
         let multiLineGame = """
           Kc 9s
           Ah
         """
         let expectedAnswer = List.fromFoldable (
          List.fromFoldable ((Card King Clubs):(Card Nine Spades):[]):
          List.fromFoldable ((Card Ace Hearts):[]):
          []
         )
         parse multiLineGame `shouldEqual` Right expectedAnswer

       it "should fail parsing a matrix of cards from an invalid multiline string" do
         let multiLineGame = """
           invalid
           Ah
         """
         parse multiLineGame `shouldEqual` Left (InvalidCard "invalid")


