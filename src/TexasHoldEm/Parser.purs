module TexasHoldEm.Parser where

import Prelude

import Data.Either (Either(..))
import Data.String.Common (split, trim)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (sequence)
import TexasHoldEm.Game (Card, GameError(..), Card(..), Rank(..), Suite(..))
import Data.Eq (notEq)
import Data.Array (filter, head)
import Data.String.CodeUnits (charAt, length, toCharArray, fromCharArray)
import Data.Maybe (Maybe)
import Data.List (fromFoldable)
import Data.List.Types (List)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Reader.Class (ask)

type MultiLineGame = String

type ParserConfig = {
   cardSeparator :: String,
   parseRank :: Char -> Either GameError Rank,
   parseSuite :: Char -> Either GameError Suite
}

defaultParserConfig = {
    cardSeparator: " ",
    parseRank: parseRank,
    parseSuite: parseSuite
}

parse = (\game -> runReader (parseMultiLineGame game) defaultParserConfig)

parseMultiLineGame :: MultiLineGame -> Reader ParserConfig (Either GameError (List (List Card)))
parseMultiLineGame game = do
  config <- ask
  let lines = split (Pattern "\n") game
  let trimedLines = trim <$> lines
  let nonEmptyLines = filter (\l-> notEq l "") trimedLines
  let parsedLines = (parseSingleLine config) <$> nonEmptyLines
  pure (sequence (fromFoldable parsedLines))

parseSingleLine :: ParserConfig -> String -> Either GameError (List Card)
parseSingleLine config line = do
  let cards = split (Pattern config.cardSeparator) line
  let parsedCards = (parseCard config) <$> cards
  sequence (fromFoldable parsedCards)

parseCard :: ParserConfig -> String -> Either GameError Card
parseCard config card = parseCardArray (toCharArray card)
  where parseCardArray [first,second] = pure (Card) <*> config.parseRank first <*> config.parseSuite second
        parseCardArray diff = Left (InvalidCard $ fromCharArray diff)

parseRank :: Char -> Either GameError Rank
parseRank 'A' = Right Ace
parseRank '2' = Right Two
parseRank '3' = Right Three
parseRank '4' = Right Four
parseRank '5' = Right Five
parseRank '6' = Right Six
parseRank '7' = Right Seven
parseRank '8' = Right Eight
parseRank '9' = Right Nine
parseRank 'T' = Right Ten
parseRank 'J' = Right Jack
parseRank 'Q' = Right Queen
parseRank 'K' = Right King
parseRank invalid = Left $ InvalidRank (fromCharArray [invalid])

parseSuite :: Char -> Either GameError Suite
parseSuite 'c' = Right Clubs
parseSuite 'd' = Right Diamonds
parseSuite 'h' = Right Hearts
parseSuite 's' = Right Spades
parseSuite invalid = Left $ InvalidSuite (fromCharArray [invalid])
