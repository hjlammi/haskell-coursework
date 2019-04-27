module Fact where

data Fact =
  PersonMovesFact PersonMoves |
  PersonMovesAwayFact PersonMovesAway |
  PersonWasBeforeFact PersonWasBefore |
  PersonTakesObjectFact PersonTakesObject |
  PersonDiscardsObjectFact PersonDiscardsObject |
  PersonHandsObjectFact PersonHandsObject |
  PersonEitherLocationFact PersonEitherLocation |
  RouteFact Route
  deriving (Show, Eq)

data PersonMoves =
  PersonMoves {
    personName :: String,
    personLocation :: String
  } deriving (Show, Eq)

data PersonMovesAway =
  PersonMovesAway {
    personMovesAwayName :: String,
    personMovesAwayLocation :: String
  } deriving (Show, Eq)

data PersonWasBefore =
  PersonWasBefore {
    personWasBeforeName :: String,
    personWasBeforeLocation :: String
  } deriving (Show, Eq)

data PersonEitherLocation =
  PersonEitherLocation {
    personEitherLocationName :: String,
    personEitherLocationLocations :: [String]
  } deriving (Show, Eq)

data PersonTakesObject =
  PersonTakesObject {
    personTakesObjectName :: String,
    personTakesObjectObject :: String
  } deriving (Show, Eq)

data PersonDiscardsObject =
  PersonDiscardsObject {
    personDiscardsObjectName :: String,
    personDiscardsObjectObject :: String
  } deriving (Show, Eq)

data PersonHandsObject =
  PersonHandsObject {
    personHandsObjectName :: String,
    personGetsObjectName :: String,
    personGetsObjectObject :: String
  } deriving (Show, Eq)

data Route =
  Route {
    roomFrom :: String,
    roomTo :: String,
    direction :: String
  } deriving (Show, Eq)
