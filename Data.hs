module Data where
import Object
import Person
import qualified Data.Map.Strict as Map

data Data =
  Data {
    persons :: Map.Map String Person
  } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves |
  PersonTakesObjectFact PersonTakesObject
  deriving (Show, Eq)

data Question =
  Question {
    subject :: String,
    place :: Maybe String
  } deriving (Show, Eq)
