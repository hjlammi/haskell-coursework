module Person where
import Object
import Fact
import qualified Data.Map.Strict as Map
import Data.Function ((&))

data Person =
  Person {
    name :: String,
    currentLocation :: [String],
    locationHistory :: [[String]],
    objects :: [String]
  } deriving (Show, Eq)

updateLocation :: Person -> [String] -> Person
updateLocation oldPerson newLocation =
  (Person.Person (name oldPerson) newLocation ((locationHistory oldPerson ++ [newLocation])) (objects oldPerson))

removeLocation :: Person -> String -> Person
removeLocation person location
  | elem location $ Person.currentLocation person =
    let updatedLocation = discard location $ Person.currentLocation person
        name = Person.name person
        objects = Person.objects person
        locationHistory = Person.locationHistory person
    in (Person.Person name updatedLocation locationHistory objects)
  | otherwise = person

updateCurrentLocation :: [String] -> Person -> Person
updateCurrentLocation [] person =
  Person.Person (name person) [] (locationHistory person) (objects person)

updateCurrentLocation xs person =
  Person.Person (name person) xs (locationHistory person) (objects person)

updateLocationHistory :: [String] -> Person -> Person
updateLocationHistory currentLocations person =
  Person.Person (name person) (currentLocation person) ((locationHistory person) ++ [currentLocations]) (objects person)

updateLocations :: [String] -> Person -> Person
updateLocations newLocation person =
  person &
  updateCurrentLocation newLocation &
  updateLocationHistory newLocation

addLocation :: Person -> [String] -> Person
addLocation person xs =
  let name = Person.name person
      objects = Person.objects person
      locations = Person.currentLocation person
      locationHistory = (Person.locationHistory person) ++ [xs]
  in Person.Person name (locations ++ xs) locationHistory objects

updateObjects :: Person -> String -> Person
updateObjects oldPerson object =
  (Person.Person (name oldPerson) (currentLocation oldPerson) (locationHistory oldPerson) ((objects oldPerson) ++ [object]))

discardObject :: Person -> PersonDiscardsObject -> Person
discardObject person fact =
  let name = Fact.personDiscardsObjectName fact
      discardedObject = Fact.personDiscardsObjectObject fact
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
