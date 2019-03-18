module Person where
import Object

data Person = Person {
                      name :: String,
                      location :: Maybe String,
                      object :: Maybe Object
                     } deriving (Show, Eq)

getName :: Person -> Maybe String
getName (Person {name = n, location = _, object = _}) = Just n

setLocation :: Person -> String -> Person
setLocation person l = person {location = Just l}

getLocation :: Person -> Maybe String
getLocation (Person {location = Just l}) = Just l

-- setObject :: Person -> String -> Person
-- setObject person o = person {object = o}
--
-- getObject :: Person -> Maybe String
-- getObject (Person {object = o}) = Just o
