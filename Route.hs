module Route where
import qualified Data.Map.Strict as Map
import Data.Function ((&))

data Direction = North | East | South | West deriving (Show, Eq)

type Room = String
type Route = [Direction]
type Routes = Map.Map (Room, Room) Route
type RoutesKV = ((Room, Room), Route)

addRoom :: Room -> Routes -> Routes
addRoom room routes =
  let maybeRoute = Map.lookup (room, room) routes
  in case maybeRoute of
    Nothing -> Map.insert (room, room) [] routes
    Just route -> routes

extendRoute :: Room -> Room -> Direction -> RoutesKV -> [RoutesKV]
extendRoute newFrom newTo direction ((from, to), route)
  | newFrom == to && not (newTo == from) = [((from, to), route), ((from, newTo), route ++ [direction])]
  | newFrom == from && not (newTo == to) = [((from, to), route), ((newTo, to), [oppositeDirection direction] ++ route)]
  | otherwise = [((from, to), route)]

extendRoutes :: Room -> Room -> Direction -> Routes -> Routes
extendRoutes from to direction routes =
  routes & Map.toList & concatMap (extendRoute from to direction) & Map.fromList

directionToData :: String -> Maybe Direction
directionToData direction
  | direction == "north" = Just North
  | direction == "east" = Just East
  | direction == "south" = Just South
  | direction == "west" = Just West
  | otherwise = Nothing

oppositeDirection :: Direction -> Direction
oppositeDirection direction
  | direction == North = South
  | direction == South = North
  | direction == East = West
  | otherwise = East
