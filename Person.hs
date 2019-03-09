data Person = Person {
                      name :: String,
                      location :: String
                     } deriving (Show)

getName :: Person -> String
getName (Person {name=n}) = n

getLocation :: Person -> String
getLocation (Person {location=l}) = l
