module Person where
import Object
import qualified Data.Map.Strict as Map

data Person =
  Person {
    name :: String,
    location :: [String],
    objects :: [String]
  } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves |
  PersonMovesAwayFact PersonMovesAway |
  PersonTakesObjectFact PersonTakesObject |
  PersonDiscardsObjectFact PersonDiscardsObject |
  PersonHandsObjectFact PersonHandsObject
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

updateLocation :: Person -> String -> Person
updateLocation oldPerson newLocation =
  (Person.Person (name oldPerson) [newLocation] (objects oldPerson))

removeLocation :: Person -> String -> Person
removeLocation person location
  | elem location $ Person.location person =
    let updatedLocation = discard location $ Person.location person
        name = Person.name person
        objects = Person.objects person
    in (Person.Person name updatedLocation objects)
  | otherwise = person

updateObjects :: Person -> String -> Person
updateObjects oldPerson object =
  (Person.Person (name oldPerson) (location oldPerson) ((objects oldPerson) ++ [object]))

discardObject :: Person -> PersonDiscardsObject -> Person
discardObject person fact =
  let name = Person.personDiscardsObjectName fact
      discardedObject = Person.personDiscardsObjectObject fact
      location = Person.location person
      objects = Person.objects person in
  if elem discardedObject objects
    then
      let newObjects = discard discardedObject objects
      in (Person.Person name location newObjects)
    else
      person

discard :: String -> [String] -> [String]
discard object [] = []
discard object [x]
  | object == x = []
  | otherwise = [x]
discard object (x:xs)
  | object == x = xs
  | otherwise = x : (discard object xs)

countObjects :: Person -> Int
countObjects person =
  length $ Person.objects person
