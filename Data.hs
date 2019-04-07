module Data where
import Object
import Person
import qualified Data.Map.Strict as Map

data Data =
  Data {
    persons :: Map.Map String Person
  } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves deriving (Show, Eq)

data Question =
  Question {
    who :: String,
    place :: String
  } deriving (Show, Eq)
