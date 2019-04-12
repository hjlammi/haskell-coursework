module Person where
import Object
import qualified Data.Map.Strict as Map

data Person =
  Person {
    name :: String,
    location :: Maybe String,
    objects :: [String]
  } deriving (Show, Eq)

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
