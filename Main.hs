module CatanProject where
import System.Random
import System.Random.Shuffle (shuffle')

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

-- A move consists a player and each phase
-- An initial game also counts as a move
data SCMove
 = Player SCRollPhase SCResourcesPhase SCTradePhase SCBuildPhase 
 | InitBoard

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

-- ResourceSet is number of Grain, Bricks, Sheep, Wood, and Ore
type ResourceSet = (Int, Int, Int, Int, Int)

type SCBuildPhase = [(Player, Building, ResourceSet)]

-- This represents adding a buidling to the board
-- Either place a new settlement or a new city at Node ID
data Building
  = NewSettlement Int
  | NewCity Int

data SCLand =
  Water | Desert |  Land Resource

data Resource = 
  Brick | Grain | Wood | Ore
  deriving(Show)

-- tools to build up the initial board

initType a 
  | a <= 4 = Brick
  | a <= 8 = Grain
  | a <= 12 = Wood
  | otherwise = Ore

init_typelist = [initType x | x <- [1..16]] 
 
-- Note
-- this isn't really random, but we could change the seed up
shuffle_seed = 10

shuffled_typelist seed = 
  shuffle' init_typelist (length init_typelist) (mkStdGen seed)

type SCBoard = (Int, [SCNode], [SCEdge])

-- an edge connects nodes on the board
-- an edge can either be a road edge
-- which has an id, maybe a road, and two connected noddes
-- or it can be a landEdge, which has an ID
-- a number representing its dice
--and a list of connected nodes 6
data SCEdge
  = RoadEdge Int MaybeSCRoad Int Int
  | LandEdge Int SCLand Int [Int]

data MaybeSCRoad = NoRoad | ARoad Player
data MaybeSCBuilding = NoBuilding| Settlement Player | City Player

-- An SCNode has an ID and possibly a building
type SCNode = (Int, MaybeSCBuilding)

emptyboard :: SCBoard
emptyboard = (1, [], [])
