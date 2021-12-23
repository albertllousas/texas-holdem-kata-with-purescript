module TexasHoldEm.Generic where

import Prelude
import Effect.Exception.Unsafe (unsafeThrow)
import Data.List.Types (List)
import Data.List.Types (List(..))
import Data.List (union)
import Data.List as List
import Data.List hiding (head, tail)
import Data.Enum (fromEnum)
import Data.List (fromFoldable)
import Data.Enum

head (h:t) = h
head _ = unsafeThrow "empty list"

tail (h:t) = t
tail _ = Nil

kCombinations :: forall a. Show a => Eq a => Int -> List a -> List (List a)
kCombinations 0 _ = Nil : Nil
kCombinations _ Nil =  Nil
kCombinations k (h:t) = withHead `union` withoutHead
  where withHead = map (Cons h) (kCombinations (k-1) t)
        withoutHead = kCombinations k t

isConsecutive :: forall a. BoundedEnum a => Eq a => List a -> Boolean
isConsecutive (x:y:xs) = (fromEnum x == (fromEnum y) -1) && isConsecutive xs
isConsecutive _ = true

listOf :: forall a. Array a -> List a
listOf array = List.fromFoldable array
