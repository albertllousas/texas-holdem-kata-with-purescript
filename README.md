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
Acceptance
  Account
    should print the statement after some money movements
Account
  Balance
    should calculate the balance for an empty list of account entries
    should calculate the balance for a list of an account entries
  Deposit
    should deposit a valid amount
    should fail depositing a negative amount
  Withdraw
    should withdraw a valid amount
    should fail withdrawing a negative amount
    should fail withdrawing when there is not enough founds
  Bank Statement
    should generate the bank statement for an account

Finished in 0.0025 seconds
9 examples, 0 failures
```