module CatanProject where
import System.Random
import System.Random.Shuffle (shuffle')
import Data.List

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
  | a <= 4 = Brick
  | a <= 8 = Grain
  | a <= 12 = Wood
  | a <= 16 = Ore
  | otherwise = Sheep

init_typelist = [initType x | x <- [1..24]] 
 
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
-- so (Up, UR, DR, Down, DL, UL, LR)
type NodeList = (Int, Int, Int, Int, Int, Int)

data MaybeSCRoad = NoRoad | ARoad Player 
  deriving(Show)

data MaybeSCBuilding = NoBuilding| Settlement Player | City Player
  deriving(Show)

-- An SCNode has an ID and possibly a building
type SCNode = (Int, MaybeSCBuilding)

emptyboard :: SCBoard
emptyboard = (1, [], [], [] )

-- landtoleft retunrs the the land to the left of a given input land
-- or none if this is the leftmost
-- or none if this is not in
landtoleft :: LandEdge -> SCBoard -> Maybe LandEdge
landtoleft (_, _, _, (_, _, _, dl, ul, _)) (_, _, lands, _) =
  find (\ (_, _, _, (_, ur, dr, _, _, _)) -> ur == ul && dr == dl) lands

  
-- board with desert is a board with just a desert in it
-- based on the variation where the desert is always in the middle and always has
-- 7
boardwithdesert :: SCBoard
boardwithdesert
  = (14, [(id, NoBuilding)| id <- [2..7]], 
    [(1, Desert, 7, (2,3,4,5,6,7))], 
    (roadedgesforrange 2 8))

-- roadsforrange creates roads for a group of id numbers
-- assumption is you pass the start of a range of 6 consecutive ids
-- so this is only really useful for the first tile
roadedgesforrange nodestart startid
  = (startid + 5, NoRoad, nodestart + 5, nodestart):([(startid + x, NoRoad, nodestart + x, nodestart + 1 + x)| x <- [0..4]])
   

{-
-- build board takes a random seed and
-- generates a fresh settlers board
buildboard :: Int -> SCBoard
buildboard seed  = boardwithlist (shuffled_typelist seed) emptyboard

-- boardwithlist generateds a board from a resource list
-- it adds a new land and its associated edges and nodes
boardwithlist :: [Resource] -> SCBoard -> SCBoard
boardwithlist [] board = board
boardwithlist rlist (_, [], []) = boardwithlist rlist boardwithdesert
boardwithlist (x:xs) board = boardwithlist xs (addland board x)
-}
