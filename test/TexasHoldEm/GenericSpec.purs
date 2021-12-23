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

spec ::Spec Unit
spec =
  describe "generic functions" do

    describe "k combinations" do

      it "should create all possible combinations of k elements given a list" do
          kCombinations 3 (List.fromFoldable ["1","2","3", "4"]) `shouldEqual` Nil


