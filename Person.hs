module Person where

data Person = Person {
                      name :: String,
                      location :: Maybe String
                     } deriving (Show, Eq)

getName :: Person -> Maybe String
getName (Person {name = n, location = _}) = Just n

setLocation :: Person -> String -> Person
setLocation person l = person {location = Just l}

getLocation :: Person -> Maybe String
getLocation (Person {location = Just l}) = Just l
