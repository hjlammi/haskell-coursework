module Question where

data Question =
  PersonLocationQuestion PersonLocationQ |
  PersonLocationBeforeQuestion PersonLocationBeforeQ |
  PersonLocationAfterQuestion PersonLocationAfterQ |
  ObjectLocationQuestion ObjectLocationQ |
  NumOfObjectsQuestion NumOfObjectsQ |
  RouteQuestion RouteQ
  deriving (Show, Eq)

data PersonLocationQ =
  PersonLocationQ {
    personLocationName :: String,
    personLocationLocation :: String
  } deriving (Show, Eq)

data PersonLocationBeforeQ =
  PersonLocationBeforeQ {
    personLocationBeforeName :: String,
    personLocationBeforeLocation :: String
  } deriving (Show, Eq)

data PersonLocationAfterQ =
  PersonLocationAfterQ {
    personLocationAfterName :: String,
    personLocationAfterLocation :: String
  } deriving (Show, Eq)

data ObjectLocationQ =
  ObjectLocationQ {
    objectName :: String
  } deriving (Show, Eq)

data NumOfObjectsQ =
 NumOfObjectsQ {
   ownerName :: String
 } deriving (Show, Eq)

data RouteQ =
 RouteQ {
   routeFrom :: String,
   routeTo :: String
 } deriving (Show, Eq)
