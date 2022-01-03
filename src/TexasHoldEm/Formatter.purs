module TexasHoldEm.Formatter where

import Data.List
import Prelude

import TexasHoldEm.Game (BestCombination(..), Card(..), Hand(..), Rank(..), Suite(..))

type AllHands = (List Hand)

type Winners = (List Hand)

format :: AllHands -> Winners -> String
format allHands winners = intercalate "\n" ((\hand -> formatHand hand winners) <$> allHands)

formatHand :: Hand -> Winners -> String
formatHand (Fold allCards) _ = formatCards allCards
formatHand hand@(FinalHand (BestCombination ranking _) allCards) winners = formatCards allCards <> " " <> show ranking <> markIfWinner
  where markIfWinner = if (hand `elem` winners) then " (winner)" else ""

formatCards :: (List Card) -> String
formatCards cards = intercalate " " (formatCard <$> cards)

formatCard :: Card -> String
formatCard (Card rank suite) = (formatRank rank) <> (formatSuite suite)

formatRank :: Rank -> String
formatRank Ace = "A"
formatRank Two = "2"
formatRank Three = "3"
formatRank Four = "4"
formatRank Five = "5"
formatRank Six = "6"
formatRank Seven = "7"
formatRank Eight = "8"
formatRank Nine = "9"
formatRank Ten = "T"
formatRank Jack = "J"
formatRank Queen = "Q"
formatRank King = "K"

formatSuite :: Suite -> String
formatSuite Clubs = "c"
formatSuite Diamonds = "d"
formatSuite Hearts = "h"
formatSuite Spades = "s"
