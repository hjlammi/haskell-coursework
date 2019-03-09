data Person = Person {
                      name :: String,
                      location :: String
                     } deriving (Show)

getName :: Person -> String
getName (Person {name=n}) = n

setLocation :: Person -> String -> Person
setLocation person l = person {location=l}

getLocation :: Person -> String
getLocation (Person {location=l}) = l
