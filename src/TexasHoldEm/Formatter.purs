module TexasHoldEm.Formatter where

import Prelude
import Data.List
import TexasHoldEm.Game (Hand)
import TexasHoldEm.Game (Suite)
import TexasHoldEm.Game (Rank)
import TexasHoldEm.Game (Card(..))
import TexasHoldEm.Game (Suite(..))
import TexasHoldEm.Game (Rank(..))
import TexasHoldEm.Game (Hand(..))
import TexasHoldEm.Game (BestCombination(..))

type AllHands = (List Hand)

type Winners = (List Hand)

format :: AllHands -> Winners -> String
format allHands winners = toMultiline ((\hand -> formatHand hand winners) <$> allHands)
  where toMultiline lines = foldl (\line acc -> if(acc == "") then (line <> "\n") else (acc <> "\n" <> line) ) "" lines -- pending todo

formatHand :: Hand -> Winners -> String
formatHand (Fold allCards) _ = formatCards allCards
formatHand hand@(FinalHand (BestCombination ranking cards) allCards) winners = formatCards allCards

formatCards :: (List Card) -> String
formatCards cards = foldl (\card acc -> if(acc == "") then card else acc <> " " <> card) "" (formatCard <$> cards)

formatCard :: Card -> String
formatCard (Card rank suite) = (formatRank rank) <> (formatSuite suite)

formatRank :: Rank -> String
formatRank Ace  = "A"
formatRank Two  = "2"
formatRank Three  = "3"
formatRank Four  = "4"
formatRank Five  = "5"
formatRank Six  = "6"
formatRank Seven  = "7"
formatRank Eight  = "8"
formatRank Nine  = "9"
formatRank Ten  = "T"
formatRank Jack  = "J"
formatRank Queen  = "Q"
formatRank King  = "K"

formatSuite :: Suite -> String
formatSuite Clubs = "c"
formatSuite Diamonds = "d"
formatSuite Hearts = "h"
formatSuite Spades = "s"
