module Person where
import Object

data Person = Person {
                      name :: String,
                      location :: Maybe String,
                      object :: Maybe Object
                     } deriving (Show, Eq)
