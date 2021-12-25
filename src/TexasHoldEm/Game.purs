module TexasHoldEm.Game where

import Prelude

import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple(..), snd, fst)
import Data.Map as Map
import Data.List.Types (List, NonEmptyList)
import Data.Enum
import Data.Enum.Generic (genericCardinality, genericToEnum, genericFromEnum, genericSucc, genericPred)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Either (Either(..), note)
import Data.Traversable (sequence)
import Data.List hiding (head, tail)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Maybe (Maybe(..), isJust)
import Effect.Exception.Unsafe (unsafeThrow)
import Data.Enum (fromEnum)
import Data.List (reverse, sortBy, union, length)
import Data.List.Types (List, toList, (:)) as List
import TexasHoldEm.Generic (kCombinations, head, listOf, isConsecutive, tail)
import Data.Foldable (sum)
import Data.Int (pow)

data GameError = InvalidCard String | InvalidSuite String | InvalidRank String | HandWithNotEnoughCards (List Card)| HandWithTooManyCards (List Card)| RepeatedCard Card

data Suite = Clubs | Spades | Diamonds | Hearts

data Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

data Card = Card Rank Suite

data Ranking = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush

data BestCombination = BestCombination Ranking (List Card)

type AllCards = (List Card)

data Hand = FinalHand BestCombination AllCards | Fold AllCards

rank :: Card -> Rank
rank (Card r _) = r

suite :: Card -> Suite
suite (Card _ s) = s

createCard :: Rank -> Suite -> Card
createCard r s = Card r s

infixr 5 createCard as *

isFinalist (Fold _) = false
isFinalist (FinalHand _ _) = true

calculateHands :: List (List Card) -> Either GameError (List Hand)
-- check repeated
calculateHands xs = sequence $ calculateHand <$> xs

calculateHand :: List Card -> Either GameError Hand
calculateHand hand | length hand > 7 = Left (HandWithTooManyCards hand)
                   | length hand < 2 = Left (HandWithNotEnoughCards hand)
                   | length hand < 7 = Right $ Fold hand
                   | otherwise = do
                      let allCombinationsOf5Cards = kCombinations 5 hand
                      let allCombinationsRanked = (\combination -> BestCombination (rankCards combination) combination) <$> allCombinationsOf5Cards
                      let bestCombination  = head $ reverse $ sort allCombinationsRanked
                      Right $ FinalHand bestCombination hand

rankCards :: (List Card) -> Ranking
rankCards cards | (isAceHighStraight cards) && (isFlush cards) = RoyalFlush
                | (isStraight cards) && (isFlush cards) = StraightFlush
                | countGroupsOfRanks cards == listOf [4,1] = FourOfAKind
                | countGroupsOfRanks cards == listOf [3,2] = FullHouse
                | isFlush cards = Flush
                | isStraight cards = Straight
                | countGroupsOfRanks cards == listOf [3,1,1] = ThreeOfAKind
                | countGroupsOfRanks cards == listOf [2,2,1] = TwoPair
                | countGroupsOfRanks cards == listOf [2,1,1,1] = OnePair
                | otherwise = HighCard

countGroupsOfRanks cards = reverse $ sort $ map NonEmpty.length (groupAll (rank <$> cards))

isAceHighStraight cards = (isStraight cards) && endsWithAnAce
 where endsWithAnAce = Ace == (head $ reverse (sort $ rank <$> cards))

isStraightStartingWithAce cards = (sort $ rank <$> cards) == (Two:Three:Four:Five:Ace:Nil)

isStraight cards = ranksAreConsecutive || isStraightStartingWithAce cards
  where ranksAreConsecutive = isConsecutive (sort $ reverse $ rank <$> cards)

isFlush cards = 1 == (length $ nub $ suite <$> cards)

determineWinners :: List Hand  -> List Hand
determineWinners hands = filter (\hand -> isAWinner hand (opponentsOf hand)) hands
  where opponentsOf hand = delete hand hands
        isAWinner  (Fold _) _ = false
        isAWinner hand opponents = if(hand >= (bestOpponent opponents)) then true else false
        bestOpponent opponents = head $ reverse $ sort opponents

instance compareHands :: Ord Hand where
   compare (Fold _) (Fold _) = EQ
   compare (Fold _) (FinalHand _ _) = LT
   compare (FinalHand _ _) (Fold _) = GT
   compare (FinalHand bc1 _) (FinalHand bc2 _) = compare bc1 bc2

instance compareBestCombination :: Ord BestCombination where
  compare (BestCombination r1 cs1) (BestCombination r2 cs2) = if (r1 == r2) then breakTie cs1 cs2 else compare r1 r2
    where breakTie cs1 cs2 = compare (scoreCards cs1) (scoreCards cs2)
          multiplyRepeated g = if((NonEmpty.length g) == 1) then sum g else sum (map (\x -> pow x 4) g)
          scoreCards cards = if (isStraightStartingWithAce cards) then 15
                             else sum (map multiplyRepeated (groupAll (cards <#> rank <#> fromEnum )))

-- painful deriving in
derive instance ordRanking :: Ord Ranking
derive instance eqRanking :: Eq Ranking
derive instance eqHand :: Eq Hand
derive instance eqBestCombination :: Eq BestCombination
derive instance genericRanking :: Generic Ranking _
derive instance genericBestCombination :: Generic BestCombination _
instance showRanking :: Show Ranking where show = genericShow
instance showBestCombination :: Show BestCombination where show = genericShow
derive instance genericHand :: Generic Hand _
instance showHand :: Show Hand where show = genericShow
instance enumRanking ∷ Enum Ranking where
  succ = genericSucc
  pred = genericPred
instance boundedRanking ∷ Bounded Ranking where
  top = genericTop
  bottom = genericBottom
instance boundedEnumRanking ∷ BoundedEnum Ranking where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
derive instance ordSuite :: Ord Suite
derive instance ordRank:: Ord Rank
derive instance eqSuite :: Eq Suite
derive instance eqRank :: Eq Rank
derive instance eqCard :: Eq Card
derive instance genericSuite :: Generic Suite _
instance showSuite :: Show Suite where show = genericShow
derive instance genericRank :: Generic Rank _
instance showRank :: Show Rank where show = genericShow
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where show = genericShow
instance enumRank ∷ Enum Rank where
  succ = genericSucc
  pred = genericPred
instance boundedRank ∷ Bounded Rank where
  top = genericTop
  bottom = genericBottom
instance boundedEnumRank ∷ BoundedEnum Rank where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum
derive instance eqGameError :: Eq GameError
derive instance genericGameError :: Generic GameError _
instance showGameError :: Show GameError where show = genericShow
