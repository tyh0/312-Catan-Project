module CatanProject where
import System.Random
import System.Random.Shuffle (shuffle')
import Data.List
import Data.Maybe

-- Settlers

-- a turn has mulitple
-- stages, some of which might
-- include all players
-- so that makes it a little more
-- complex than just a simple set of
-- moves

-- there can be up to 4 players
data  Player 
  = Player1 | Player2 | Player3 | Player4
  deriving(Show)

-- A move consists a player and each phase
-- An initial game also counts as a move
data SCMove
 = Player SCRollPhase SCResourcesPhase SCTradePhase SCBuildPhase 
 | InitBoard
  deriving(Show)

-- The Roll phase is just an integer
-- The result of 2d6 being rolled
type SCRollPhase = Int

-- The Resource Phase is a list of resources
-- added to each player
type SCResourcesPhase  = [(Player, ResourceSet)]

-- The Trade phase is a list of Trades
type SCTradePhase = [Trade]

-- A trade consists of
-- Player1
-- Player2 (or a port or the bank)
-- ResourceSet1  (resources going from P1 to P2)
-- ResourceSet2  (resources going from P2 to P1)
type Trade = (Player, TradeEntity, ResourceSet, ResourceSet)
  
data TradeEntity = TEPlayer Player | Port | Bank
  deriving(Show)

-- ResourceSet is number of Grain, Bricks, Sheep, Wood, and Ore
type ResourceSet = (Int, Int, Int, Int, Int)

type SCBuildPhase = [(Player, Building, ResourceSet)]

-- This represents adding a buidling to the board
-- Either place a new settlement or a new city at Node ID
data Building
  = NewSettlement Int
  | NewCity Int
  deriving(Show)

data SCLand =
  Desert |  Land Resource
  deriving(Show)

data Resource = 
  Brick | Grain | Wood | Ore | Sheep
  deriving(Show)

-- tools to build up the initial board

initType a 
  | a <= 4 = Land Brick
  | a <= 8 = Land Grain
  | a <= 12 = Land Wood
  | a <= 16 = Land Ore
  | otherwise = Land Sheep

init_typelist = (Desert:[initType x | x <- [1..24]])
 
-- Note
-- this isn't really random, but we could change the seed up
shuffle_seed = 10

shuffled_typelist seed = 
  shuffle' init_typelist (length init_typelist) (mkStdGen seed)

type SCBoard = (Int, [SCNode], [LandEdge], [RoadEdge])

-- an edge connects nodes on the board
-- we have 2 types of edges
-- Road Edge
-- which has an id, maybe a road, and two connected noddes
type RoadEdge = (Int, MaybeSCRoad, Int, Int)

-- Land Edge which has an ID
-- a number representing its dice
-- and a group of connected nodes (as ids)
type LandEdge = (Int, SCLand, Int, NodeList)
-- A nodelist is a tuple of 6 ids
-- starting from top and moving clockwise
-- so (Up, UR, DR, Down, DL, UL)
type NodeList = (Int, Int, Int, Int, Int, Int)

data MaybeSCRoad = NoRoad | ARoad Player 
  deriving(Show)

data MaybeSCBuilding = NoBuilding| Settlement Player | City Player
  deriving(Show)

-- An SCNode has an ID and possibly a building
type SCNode = (Int, MaybeSCBuilding)

emptyboard :: SCBoard
emptyboard = (1, [], [], [] )

nodeforid :: Int -> SCBoard -> Maybe SCNode
nodeforid id (_, nodes, _, _) 
  = find (\ (nid, _) -> nid == id) nodes

landforid :: Int -> SCBoard -> Maybe LandEdge
landforid id (_, _, lands, _)
  = find (\ (lid, _, _, _) -> lid == id) lands

roadedgeforid :: Int -> SCBoard -> Maybe RoadEdge
roadedgeforid id  (_, _, _, roads)
  = find (\ (rid, _, _, _) -> rid == id) roads

-- landtoleft returns the the land to the left of a given input land
-- or none if this is the leftmost
-- or none if this is not in
landtoleft :: LandEdge -> SCBoard -> Maybe LandEdge
landtoleft (_, _, _, (_, _, _, _, dl, ul)) (_, _, lands, _) =
  find (\ (_, _, _, (_, ur, dr, _, _, _)) -> ur == ul && dr == dl) lands

-- landtoright returns the the land to the right of a given input land
-- or none if this is the right edge
-- or none if this is not in
landtoright :: LandEdge -> SCBoard -> Maybe LandEdge
landtoright (_, _, _, (_, ur, dr, _, _, _)) (_, _, lands, _) =
  find (\ (_, _, _, (_, _, _, _, dl, ul)) -> ur == ul && dr == dl) lands

-- landtoupright returns the land up and to the right
-- or none if this is at an edge of the board
landtoupright :: LandEdge -> SCBoard -> Maybe LandEdge
landtoupright (_, _, _, (top, ur, _, _, _, _)) (_, _, lands, _) =
  find (\ (_, _, _, (_, _, _, down, dl, _)) -> top == dl && ur == dl) lands

-- landtodownleft returns the land down and to the left
-- or none if this is at an edge of the board
landtodownleft :: LandEdge -> SCBoard -> Maybe LandEdge
landtodownleft (_, _, _, (_, _, _, down, dl, _)) (_, _, lands, _) =
  find (\ (_, _, _, (top, ur, _, down, dl, _)) -> top == dl && ur == dl) lands

-- landtodownright returns the land down and to the right
-- or none if this is at an edge of the board
landtodownright :: LandEdge -> SCBoard -> Maybe LandEdge
landtodownright (_, _, _, (_, _, dr, down, _, _)) (_, _, lands, _) =
  find (\ (_, _, _, (top, _, _, _, _, ul)) -> down == ul && dr == top) lands

-- landtoupleft returns the land down and to the left
-- or none if this is at an edge of the board
landtoupleft :: LandEdge -> SCBoard -> Maybe LandEdge
landtoupleft (_, _, _, (top, _, _, _, _, ul)) (_, _, lands, _) =
  find (\ (_, _, _, (_, _, dr, down, _, _)) -> down == ul && dr == top) lands
  
-- roadsforrange creates roads for a group of id numbers
-- assumption is you pass the start of a range of 6 consecutive ids
-- so this is only really useful for the first tile
roadedgesforrange nodestart startid
  = (startid + 5, NoRoad, nodestart + 5, nodestart):([(startid + x, NoRoad, nodestart + x, nodestart + 1 + x)| x <- [0..4]])
   

-- build board takes a random seed and
-- generates a fresh settlers board
buildboard :: Int -> SCBoard
buildboard seed  
  = boardwithlist (shuffled_typelist seed) starting_tile_numbers  starting_directions emptyboard

starting_tile_numbers = [5, 2, 6, 3, 8, 10, 9, 12, 11, 4, 8,
  10, 9, 4, 5, 6, 3, 11]

-- boardwithlist generateds a board from a resource list
-- it adds a new land and its associated edges and nodes
boardwithlist :: [SCLand] -> [Int] -> [Direction] -> SCBoard -> SCBoard
boardwithlist [] _ _ board = board
boardwithlist (Desert:rs) tlist (d:ds) board = 
  boardwithlist rs tlist ds (addland board Desert 7 d)
boardwithlist (r:rs) (x:xs) (d:ds) board = boardwithlist rs xs ds (addland board r x d)

-- lands get added in a spiral
data Direction = UR | R | DR | DL | L | UL
  deriving(Show)

-- next direction going clockwise
nextdir :: Direction -> Direction
nextdir UR = R
nextdir R = DR
nextdir DR = DL
nextdir DL = L
nextdir L = UL
nextdir UL = UR

-- directions to add
starting_directions = 
  [DL, -- the first direction can be anything
   DL, DL, DR, DR, R, R, UR, UR, UL, UL, L, -- outer ring
   DL, DL, DR, R, UR, UL, L, -- inner ring
   DL] -- center

-- find the last land added in a board (the one with the highest id)
lastlandadded :: SCBoard -> Maybe LandEdge
lastlandadded (_, _, lands, _) 
  = foldl lastlandhelper Nothing lands

-- True if the first landedge has a higher id
higherid :: LandEdge -> LandEdge -> Bool
higherid (id1, _, _, _) (id2, _, _, _) 
  | id1 > id2 = True
  | otherwise = False

-- helper for finding the max land
lastlandhelper :: Maybe LandEdge -> LandEdge -> Maybe LandEdge
lastlandhelper Nothing any = Just any
lastlandhelper (Just l1) l2
  | higherid l1 l2 = Just l1
  | otherwise = Just l2

addland :: SCBoard -> SCLand -> Int -> Direction -> SCBoard
addland board land rollno dir =
  let prev = lastlandadded board in 
    case prev of Nothing -> addfirstland land rollno
                 Just pl -> addnextland board land rollno pl dir


-- add first land to a brand new board
addfirstland :: SCLand -> Int -> SCBoard
addfirstland land rollno
  = (14, [(id, NoBuilding)| id <- [2..7]], 
    [(1, land, rollno, (2,3,4,5,6,7))], 
    (roadedgesforrange 2 8))

addnextland :: SCBoard -> SCLand -> Int -> LandEdge -> Direction -> SCBoard
-- TODO stub
addnextland _ _ _ _ _ = emptyboard
