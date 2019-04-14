module Person where
import Object
import qualified Data.Map.Strict as Map

data Person =
  Person {
    name :: String,
    location :: Maybe String,
    objects :: [String]
  } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves |
  PersonTakesObjectFact PersonTakesObject |
  PersonDiscardsObjectFact PersonDiscardsObject
  deriving (Show, Eq)

data PersonMoves =
  PersonMoves {
    personName :: String,
    personLocation :: String
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

updateLocation :: PersonMoves -> Person
updateLocation fact =
  let name = Person.personName fact
      location = Person.personLocation fact in
  (Person.Person name (Just location) [])
