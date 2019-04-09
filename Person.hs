module Person where
import Object

data Person =
  Person {
    name :: String,
    location :: Maybe String,
    object :: Maybe String
  } deriving (Show, Eq)

data PersonMoves =
  PersonMoves {
    personName :: String,
    personLocation :: String
  } deriving (Show, Eq)

data PersonMovesObject =
  PersonMovesObject {
    personMovesObjectName :: String,
    personMovesObjectObject :: String
  } deriving (Show, Eq)
