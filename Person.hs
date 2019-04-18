module Person where
import Object
import qualified Data.Map.Strict as Map

data Person =
  Person {
    name :: String,
    currentLocation :: [String],
    locationHistory :: [String],
    objects :: [String]
  } deriving (Show, Eq)

data Fact =
  PersonMovesFact PersonMoves |
  PersonMovesAwayFact PersonMovesAway |
  PersonTakesObjectFact PersonTakesObject |
  PersonDiscardsObjectFact PersonDiscardsObject |
  PersonHandsObjectFact PersonHandsObject |
  PersonEitherLocationFact PersonEitherLocation
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

updateLocation :: Person -> String -> Person
updateLocation oldPerson newLocation =
  (Person.Person (name oldPerson) [newLocation] ((locationHistory oldPerson ++ [newLocation])) (objects oldPerson))

removeLocation :: Person -> String -> Person
removeLocation person location
  | elem location $ Person.currentLocation person =
    let updatedLocation = discard location $ Person.currentLocation person
        name = Person.name person
        objects = Person.objects person
        locationHistory = Person.locationHistory person
    in (Person.Person name updatedLocation locationHistory objects)
  | otherwise = person

addLocation :: Person -> [String] -> Person
addLocation person xs =
  let name = Person.name person
      objects = Person.objects person
      locations = Person.currentLocation person
      locationHistory = (Person.locationHistory person) ++ xs
  in Person.Person name (locations ++ xs) locationHistory objects

updateObjects :: Person -> String -> Person
updateObjects oldPerson object =
  (Person.Person (name oldPerson) (currentLocation oldPerson) (locationHistory oldPerson) ((objects oldPerson) ++ [object]))

discardObject :: Person -> PersonDiscardsObject -> Person
discardObject person fact =
  let name = Person.personDiscardsObjectName fact
      discardedObject = Person.personDiscardsObjectObject fact
      location = Person.currentLocation person
      locationHistory = Person.locationHistory person
      objects = Person.objects person in
  if elem discardedObject objects
    then
      let newObjects = discard discardedObject objects
      in (Person.Person name location locationHistory newObjects)
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
