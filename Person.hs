data Person = Person {
                      name :: String
                     } deriving (Show)

getName :: Person -> String
getName (Person {name=n}) = n
