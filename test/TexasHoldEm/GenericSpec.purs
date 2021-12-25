module TexasHoldEm.RankSpec where

import Prelude hiding ((*))

import Data.List ((:))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TexasHoldEm.Generic (kCombinations)
import Data.List as List
import Data.List.Types (List(..))
import TexasHoldEm.Generic (isConsecutive)
import TexasHoldEm.Game (Rank(..))

spec ::Spec Unit
spec =
  describe "generic functions" do

    describe "k combinations" do

      it "should create all possible combinations of k elements given a list" do
        let combinations = (1:2:3:Nil):(1:2:4: Nil):(1:3:4: Nil):(2:3:4: Nil):Nil
        kCombinations 3 (1:2:3:4:Nil) `shouldEqual` combinations

      it "should create only one combination when k has the same size of the list" do
        let combinations = (1:2:3:Nil):Nil
        kCombinations 3 (1:2:3:Nil) `shouldEqual` combinations

    describe "is consecutive" do

      it "should check that a list of enums is consecutive" do
        isConsecutive (Two:Three:Four:Nil) `shouldEqual` true

      it "should check that a list of enums is not consecutive" do
        isConsecutive (Two:Three:Five:Nil) `shouldEqual` false
