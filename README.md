# texas-holdem-kata-with-purescript

[Texas HoldEm kata](https://codingdojo.org/kata/TexasHoldEm/) in haskell using [outside-in TDD with double loop](http://coding-is-like-cooking.info/2013/04/outside-in-development-with-double-loop-tdd/).

## The problem to solve
Given
```
Kc 9s Ks Kd 9d 3c 6d
9c Ah Ks Kd 9d 3c 6d
Ac Qc Ks Kd 9d 3c
9h 5s
4d 2d Ks Kd 9d 3c 6d
7s Ts Ks Kd 9d
```
Generate
```
Kc 9s Ks Kd 9d 3c 6d Full House (winner)
9c Ah Ks Kd 9d 3c 6d Two Pair
Ac Qc Ks Kd 9d 3c 
9h 5s 
4d 2d Ks Kd 9d 3c 6d Flush
7s Ts Ks Kd 9d 
```


## The solution

[Here](/test/AcceptanceSpec.hs), the acceptance test for the exercise.
```purescript
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
        Kc 9s Ks Kd 9d 3c 6d FullHouse (winner)
        9c Ah Ks Kd 9d 3c 6d TwoPair
        Ac Qc Ks Kd 9d 3c
        9h 5s
        4d 2d Ks Kd 9d 3c 6d Flush
        7s Ts Ks Kd 9d
      """
      output `shouldEqual` (Right $ inline expectedOutput)
```

[Here](/test/TexasHoldEm), the unit tests.

[Here](/src/TexasHoldEm), the solution.


## Run tests
```shell
spago test
```
```shell
Texas Hold’Em Championship show
  ✓︎ should knew what hands the players were holding and which hand won the round
format
  ✓︎ should format a Rank
  ✓︎ should format a suite
  ✓︎ should format a card 
  ✓︎ should format a list of cards
  ✓︎ should format a folded hand
  ✓︎ should format a non winner final hand
  ✓︎ should format a winner final hand
  ✓︎ should format a list of hands
game » rank cards
  ✓︎ should rank a hand with an ace, king, queen, jack, ten, all in the same suit, as a Royal Flush
  ✓︎ should rank a hand that contains five cards of sequential rank, all of the same suit, as a Straight Flush
  ✓︎ should rank a hand that contains four cards of one rank as a Four of a Kind
  ✓︎ should rank a hand that contains three cards of one rank and two cards of another rank, as a Full House
  ✓︎ should rank a hand that contains five cards all of the same suit, not all of sequential rank, as a Fush
  ✓︎ should rank a hand that contains five cards of sequential rank, not all of the same suit, as a Straight
  ✓︎ should rank a hand that contains three cards of one rank and two other cards, as a Three of a Kind
  ✓︎ should rank a hand with two cards of one rank, two cards of another rank and other card, as a Two Pair 
  ✓︎ should rank a hand that contains two cards of one rank and three cards of three other ranks, as a One Pair
  ✓︎ should rank a hand that does not fall into any other category, as a High Card
game » calculate a hand given seven cards (two hole cards and five community cards)
  ✓︎ calculate the best hand when there are multiple combinations to rank
  ✓︎ should fold when there are not enough community cards (less than five)
  ✓︎ should fail when there are not enough hole cards
  ✓︎ should fail when there are too many community cards
game » determine winners
  ✓︎ shouldn't be any winners when all the hands are Fold
  ✓︎ should determine the winners when there are multiple final hands
game » compare hands
  ✓︎ two folded hands should be equal
  ✓︎ a final hand should be greater than a fold
  ✓︎ two final hands should compared using their best combinations
game » compare best combinations of a two different hands (ranking with their corresponding cards)
  ✓︎ should compare by rank when rankins are different
  ✓︎ if two hands hold Royal Flush, the pot is divided
  ✓︎ if two hands hold the same straight flush, the pot is divided
  ✓︎ if two hands hold straight flush, then the higher ranking straight flush wins
  ✓︎ if two hands hold the same Four of a kind with the same kicker, the pot is divided
  ✓︎ if two hands hold Four of a kind, the hand with the highest-ranking four of a kind wins
  ✓︎ if two hands hold the same Four of a kind, the hand with the highest-ranking kicker wins
  ✓︎ if two hands hold the same Full House, the pot is divided
  ✓︎ if two hands hold Full House, the hand with the highest three of a kind wins
  ✓︎ if two hands hold Full House with the same three of a kind, the hand with the highest pair wins
  ✓︎ if two hands hold the same Flush, the pot is divided
  ✓︎ if two hands hold Flush, the hand with the highest-ranking flush wins
  ✓︎ if two hands hold the same straight, the pot is divided
  ✓︎ if two hands hold straight flush, then the higher ranking straight flush wins
  ✓︎ if two hands hold an identical three of a kind with the same kickers, the pot is divided
  ✓︎ if two hands hold an identical three of a kind, then the hand with the 4th strongest card wins
  ✓︎ if two hands hold an identical three of a kind and also the same 4th strongest card, then the strongest card 5th card will decide
  ✓︎ if two hands hold two pairs each, the hand with the larger top pair will win
  ✓︎ if two hands hold two pairs each with the same higher pair, then the pair with the highest second pair wins
  ✓︎ if two hands hold two identical pairs each, the hand with the highest 5th card wins
  ✓︎ if two hands hold two identical pairs each with the same 5th card, the pot is divided
  ✓︎ if two hands hold pairs each, the hand with the larger pair will win
  ✓︎ if two hands hold two identical pairs each, the hand with the best kickers wins
  ✓︎ if two hands hold the same identical five cards that are not matching any ranking, the pot is Shared
  ✓︎ if two hands hold five cards that are not matching any ranking, the hand with the first different highest card wins
parse » parse rank
  ✓︎ should parse a rank from valid character
  ✓︎ should fail parsing a rank from an invalid character
parse » parse suite
  ✓︎ should parse a suite from valid character
  ✓︎ should fail parsing a suite from an invalid character
parse » parse card
  ✓︎ should parse a card from a valid string
  ✓︎ should fail parsing a card from an empty string
  ✓︎ should fail parsing a card from an invalid string
  ✓︎ should fail parsing a card from an invalid rank
  ✓︎ should fail parsing a card from an invalid suite
parse » parse a list of cards
  ✓︎ should parse a list of cards from a valid string
  ✓︎ should fail parsing a list of cards from an invalid string
parse » parse a matrix of cards
  ✓︎ should parse a matrix of cards from a valid multiline string
  ✓︎ should fail parsing a matrix of cards from an invalid multiline string
generic functions » k combinations
  ✓︎ should create all possible combinations of k elements given a list
  ✓︎ should create only one combination when k has the same size of the list
generic functions » is consecutive
  ✓︎ should check that a list of enums is consecutive
  ✓︎ should check that a list of enums is not consecutive

Summary
70/70 tests passed

```