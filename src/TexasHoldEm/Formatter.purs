module TexasHoldEm.Formatter where

import Prelude
import Data.List
import TexasHoldEm.Game (Hand)

type AllHands = (List Hand)

type Winners = (List Hand)

format :: AllHands -> Winners -> String
format allHands winners = ""

