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
-- data  Player 
--   = Player1 | Player2 | Player3 | Player4
--   deriving(Show)

-- A player has: 
-- A set of buildable tokens (15 roads, 3 settlements, 3 cities)
-- A hand of resource cards
-- A collection of development cards in hand
-- A collection of development cards played
-- A turn order
-- A number of victory points

data Player = Player
  { id :: Int -- Player number (eg: Player 1, Player 2, etc)
  , turn :: Int -- Assigned at the beginning of the game
  , resourcehand :: ResourceHand -- Resources the player has
  , devcardhand :: DevCardHand -- Unplayed dev cards
  , activedevcards :: DevCardHand -- dev cards player has used, eg: 3 knights
  , victorypoints :: Int -- total victory points
  } deriving (Show,Eq)

-- a player's hand of resources is a list of resource cards
type ResourceHand
 = [ResourceCard] 

-- a players devcard hand is a list of development cards
type DevCardHand 
 = [DevCard] 

-- a resource card has an id Int, and a Resource type
type ResourceCard
 = (Int, Resource) 

-- a resource card has an id Int and a card type
-- types are: 
-- knight
-- victory point
-- road building
-- monopoly
-- year of plenty
data DevCardValue = K | VP | RB | M | YOP
 deriving(Show, Eq)

type DevCard
 = (Int, DevCardValue)

-- A move consists a player and each phase
-- An initial game also counts as a move
data SCMove
 = MovePlayer SCRollPhase SCResourcesPhase SCTradePhase SCBuildPhase 
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
  
data TradeEntity = TEPlayer Player | SCPort | Bank
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

data SCResource =
  Desert |  Land Resource | Water SCPort
  deriving(Show)

data SCPort = 
  Wildcard | PBrick | PGrain | PWood | POre | PSheep
  deriving(Show)

data Resource = 
  Brick | Grain | Wood | Ore | Sheep
  deriving(Show, Eq)

-- tools to build up the initial board

initType a 
  | a <= 3 = Land Brick
  | a <= 7 = Land Grain
  | a <= 11 = Land Wood
  | a <= 14 = Land Ore
  | otherwise = Land Sheep

init_typelist = (Desert:[initType x | x <- [1..18]])
 
-- Note
-- this isn't really random, but we could change the seed up
shuffle_seed = 10

shuffled_typelist seed = 
  shuffle' init_typelist (length init_typelist) (mkStdGen seed)

-- a board consists of: 
-- a boardID int,
-- a list of nodes [SCNode],
-- a list of HexTiles that span those nodes, [HexTile],
-- a list of road edges that span those nodes, [RoadEdge]
type SCBoard = (Int, [SCNode], [HexTile], [RoadEdge])

-- an edge connects nodes on the board
-- we have 2 types of edges
-- Road Edge
-- which has an id, maybe a road, and two connected noddes
type RoadEdge = (Int, MaybeSCRoad, Int, Int)

-- Land Edge which has an ID
-- a number representing its dice
-- and a group of connected nodes (as ids)
type HexTile = (Int, SCResource, Int, NodeList)
-- A nodelist is a tuple of 6 SCNode ids
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

hextileforid :: Int -> SCBoard -> Maybe HexTile
hextileforid id (_, _, hextiles, _)
  = find (\ (lid, _, _, _) -> lid == id) hextiles

roadedgeforid :: Int -> SCBoard -> Maybe RoadEdge
roadedgeforid id  (_, _, _, roads)
  = find (\ (rid, _, _, _) -> rid == id) roads

-- hextiletoleft returns the the hextile to the left of a given input hextile
-- or none if this is the leftmost
-- or none if this is not in
hextiletoleft :: HexTile -> SCBoard -> Maybe HexTile
hextiletoleft (_, _, _, (_, _, _, _, dl, ul)) (_, _, hextiles, _) =
  find (\ (_, _, _, (_, ur, dr, _, _, _)) -> ur == ul && dr == dl) hextiles

-- hextiletoright returns the the hextile to the right of a given input hextile
-- or none if this is the right edge
-- or none if this is not in
hextiletoright :: HexTile -> SCBoard -> Maybe HexTile
hextiletoright (_, _, _, (_, ur, dr, _, _, _)) (_, _, hextiles, _) =
  find (\ (_, _, _, (_, _, _, _, dl, ul)) -> ur == ul && dr == dl) hextiles

-- hextiletoupright returns the hextile up and to the right
-- or none if this is at an edge of the board
hextiletoupright :: HexTile -> SCBoard -> Maybe HexTile
hextiletoupright (_, _, _, (top, ur, _, _, _, _)) (_, _, hextiles, _) =
  find (\ (_, _, _, (_, _, _, down, dl, _)) -> top == dl && ur == dl) hextiles

-- hextiletodownleft returns the hextile down and to the left
-- or none if this is at an edge of the board
hextiletodownleft :: HexTile -> SCBoard -> Maybe HexTile
hextiletodownleft (_, _, _, (_, _, _, down, dl, _)) (_, _, hextiles, _) =
  find (\ (_, _, _, (top, ur, _, down, dl, _)) -> top == dl && ur == dl) hextiles

-- hextiletodownright returns the hextile down and to the right
-- or none if this is at an edge of the board
hextiletodownright :: HexTile -> SCBoard -> Maybe HexTile
hextiletodownright (_, _, _, (_, _, dr, down, _, _)) (_, _, hextiles, _) =
  find (\ (_, _, _, (top, _, _, _, _, ul)) -> down == ul && dr == top) hextiles

-- hextiletoupleft returns the hextile down and to the left
-- or none if this is at an edge of the board
hextiletoupleft :: HexTile -> SCBoard -> Maybe HexTile
hextiletoupleft (_, _, _, (top, _, _, _, _, ul)) (_, _, hextiles, _) =
  find (\ (_, _, _, (_, _, dr, down, _, _)) -> down == ul && dr == top) hextiles
  
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
-- it adds a new hextile and its associated edges and nodes
boardwithlist :: [SCResource] -> [Int] -> [Direction] -> SCBoard -> SCBoard
boardwithlist [] _ _ board = board
boardwithlist _ [] _ board = error "Not enough tile numbers"
boardwithlist _ _ [] board = error "Not enough directions"
boardwithlist (Desert:rs) tlist (d:ds) board = 
  boardwithlist rs tlist ds (addhextile board Desert 7 d)
boardwithlist (r:rs) (x:xs) (d:ds) board = boardwithlist rs xs ds (addhextile board r x d)

-- hextiles get added in a spiral
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

-- find the last hextile added in a board (the one with the highest id)
lasthextileadded :: SCBoard -> Maybe HexTile
lasthextileadded (_, _, hextiles, _) 
  = foldl lasthextilehelper Nothing hextiles

-- True if the first HexTile has a higher id
higherid :: HexTile -> HexTile -> Bool
higherid (id1, _, _, _) (id2, _, _, _) 
  | id1 > id2 = True
  | otherwise = False

-- helper for finding the max hextile
lasthextilehelper :: Maybe HexTile -> HexTile -> Maybe HexTile
lasthextilehelper Nothing any = Just any
lasthextilehelper (Just l1) l2
  | higherid l1 l2 = Just l1
  | otherwise = Just l2

addhextile :: SCBoard -> SCResource -> Int -> Direction -> SCBoard
addhextile board hextile rollno dir =
  let prev = lasthextileadded board in 
    case prev of Nothing -> addfirsthextile hextile rollno
                 Just pl -> addnexthextile board hextile rollno pl dir

-- add first hextile to a brand new board
addfirsthextile :: SCResource -> Int -> SCBoard
addfirsthextile hextile rollno
  = (14, [(id, NoBuilding)| id <- [2..7]], 
    [(1, hextile, rollno, (2,3,4,5,6,7))], 
    (roadedgesforrange 2 8))

-- addnexthextile has to do a lot of things
-- 1. It needs to place a new hextile on the board
-- 2. It needs to look back from the previous tile to find and nodes
--    that the hextile should be connected to (we can do this by 
--    walking around the tile from the previous tile
-- 3. It needs to add nodes and roadedges for any nodes
--    that are not already there
addnexthextile :: SCBoard -> SCResource -> Int -> HexTile -> Direction -> SCBoard
addnexthextile (id, nodes, hextiles, roads) hextile rollno prevhextile direction =
  let existingnodes = getexistingnodes prevhextile direction hextiles
      (newnodes, newnodeset) = add_needed_nodes existingnodes (id + 1)
      nodesneeded = length newnodes
      newedges = makeneedededges newnodeset roads (id + 1 + nodesneeded)
      edgesneeded = length newedges
      in (id + 1 +  nodesneeded + edgesneeded, 
        newnodes ++ nodes,
        (id, hextile, rollno, newnodeset):hextiles,
        newedges ++ roads)

-- fill in each slot in the nodeset for a hextile with 0 if there
-- is no node for it, or the node's id if there is
getexistingnodes :: HexTile -> Direction -> [HexTile]-> NodeList
-- first add the ones from the previous edge
-- then use a recursive helper to look for more
getexistingnodes le dir hextiles =
  let (sid, _, _, (up, ur, dr, dn, dl, ul)) = le
    in case dir of
      UR -> existingnodeshelper le sid (sharednodes le DL) DL hextiles
      R  -> existingnodeshelper le sid (sharednodes le L) L hextiles
      DR -> existingnodeshelper le sid (sharednodes le UL) UL hextiles
      DL -> existingnodeshelper le sid (sharednodes le UR) UR hextiles
      L  -> existingnodeshelper le sid (sharednodes le R)  R hextiles
      UL -> existingnodeshelper le sid (sharednodes le DR) DR hextiles

-- fill in a node list with just the nodes facing
-- direction on a HexTile
sharednodes :: HexTile -> Direction -> NodeList
sharednodes (_, _, _, (up, ur, dr, dn, dl,ul)) dir
  = case dir of 
      UR -> (0,0,0,ur,up,0)
      R  -> (0,ul,dl,0,0,0)
      DR -> (dr,0,0,0,0,dn)
      DL -> (dl,dn,0,0,0,0)
      L  -> (0,0,0,0,dr,ur)
      UL -> (0,0,up,ul,0,0)

-- takes a current HexTile
-- an id (this is the start, so we don't get stuck in an infinite loop
-- a direction (this time the direction of the new hextile
existingnodeshelper :: HexTile -> Int -> NodeList -> Direction -> [HexTile] -> NodeList
existingnodeshelper le sid nl dir hextiles
  = nodeunify (enodescw le sid nl dir hextiles) (enodesccw le sid nl dir hextiles)

-- choose non-zero items for each elem in a nodelist
nodeunify :: NodeList -> NodeList -> NodeList
nodeunify (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = 
  let nz = (\ a b -> if (a == 0) then a else b) in
    ((nz a1 a2),
      (nz b1 b2),
      (nz c1 c2),
      (nz d1 d2),
      (nz e1 e2),
      (nz f1 f2))

-- cw_move input_dir
-- input_dir is a direction facing in towards the centre
-- of clockwise ring
-- returns (go, in)
-- go is the next direction to go in a clockwise ring
-- in is the direction facing towards the centre
cw_move :: Direction -> (Direction, Direction)
cw_move UR = (UL, R)
cw_move R  = (UR, DR)
cw_move DR = (R,  DL)
cw_move DL = (DR, L)
cw_move L  = (DL, UL)
cw_move UL = (L, UR)

-- ccw_move input_dir
-- input_dir is a direction facing in towards the centre
-- of clockwise ring
-- returns (go, in)
-- go is the next direction to go in a clockwise ring
-- in is the direction facing towards the centre
ccw_move :: Direction -> (Direction, Direction)
ccw_move UR = (R, UL)
ccw_move UL = (UR, L)
ccw_move L  = (UL, DL)
ccw_move DL = (L, DR)
ccw_move DR = (DL, R)
ccw_move R  = (DR, UR)

-- search clockwise for points around a space
enodescw :: HexTile -> Int -> NodeList -> Direction -> [HexTile] -> NodeList
enodescw current sid acc dir hextiles
 = let (go, indir) = cw_move dir
  in case (hextileindir current dir hextiles) of
    Nothing -> acc
    Just le -> let (id, _, _,_)  = le in
      if (sid == id) then acc else (enodescw le sid 
      (nodeunify acc (sharednodes le indir)) indir hextiles)

-- search counterclockwise for points around a space
enodesccw :: HexTile -> Int -> NodeList -> Direction -> [HexTile] -> NodeList
enodesccw current sid acc dir hextiles
 = let (go, indir) = ccw_move dir
  in case (hextileindir current dir hextiles) of
    Nothing -> acc
    Just le -> let (id, _, _,_)  = le in
      if (sid == id) then acc else (enodesccw le sid 
      (nodeunify acc (sharednodes le indir)) indir hextiles)

-- get a hextile in a direction
hextileindir :: HexTile -> Direction -> [HexTile] -> Maybe HexTile
hextileindir le dir hextiles=
  let fakeboard = (0, [], hextiles, [])
  in case dir of
    UR -> hextiletoupright le fakeboard
    R  -> hextiletoright le fakeboard
    DR -> hextiletodownright le fakeboard
    DL -> hextiletodownleft le fakeboard
    L  -> hextiletoleft le fakeboard
    UL -> hextiletoupleft le fakeboard

-- given a nodeset, return a list of empty nodes for each
-- 0 value, starting with id
add_needed_nodes :: NodeList -> Int -> ([SCNode], NodeList)
add_needed_nodes nl id = add_needed_nodes_helper [] nl id
-- recursive helper
add_needed_nodes_helper :: [SCNode] -> NodeList -> Int -> ([SCNode], NodeList)
add_needed_nodes_helper acc (up, ur, dr, dn, dl, ul) id
  | up == 0 = add_needed_nodes_helper ((id, NoBuilding):acc) (id, ur, dr, dn, dl, ul) (id + 1)
  | ur == 0 = add_needed_nodes_helper ((id, NoBuilding):acc)
  (up, id, dr, dn, dl, ul) (id + 1)
  | dr == 0 = add_needed_nodes_helper ((id, NoBuilding):acc) 
  (up, ur, id, dn, dl, ul) (id + 1)
  | dn == 0 = add_needed_nodes_helper ((id, NoBuilding):acc) 
  (up, ur, dr, id, dl, ul) (id + 1)
  | dl == 0 = add_needed_nodes_helper ((id, NoBuilding):acc)
  (up, ur, dr, dn, id, ul) (id + 1)
  | ul == 0 = add_needed_nodes_helper ((id, NoBuilding):acc) 
  (up, ur, dr, dn, dl, id) (id + 1)
  | otherwise = (acc, (up, ur, dr, dn, dl, ul))

-- given a nodeset, create empty RoadEdges for any that don't exist
-- starting with id
makeneedededges :: NodeList -> [RoadEdge] -> Int -> [RoadEdge]
makeneedededges nl roads id = needededgeshelper [] (neededpairs nl) roads id

needededgeshelper :: [RoadEdge] -> [(Int, Int)] -> [RoadEdge] -> Int -> [RoadEdge]

needededgeshelper acc [] _ _ = acc

needededgeshelper acc ((n1, n2):pairs) roads id = 
  case (findroad n1 n2 roads) of
    Just _ -> needededgeshelper acc pairs roads id
    Nothing -> needededgeshelper ((id, NoRoad, n1, n2):acc) pairs roads (id + 1)

-- find a road connecting 2 points
findroad :: Int -> Int -> [RoadEdge] -> Maybe RoadEdge
findroad a b roads 
  = find (\ (_, _, x, y) -> (x == a && y == b) || (x == b && y == a)) roads

-- List all the pairs of nodes needed from a nodelist
neededpairs :: NodeList -> [(Int, Int)]
neededpairs (a, b, c, d, e,f)
  = [(a,b), (b, c), (c,d), (d,e), (e,f), (f,a)]

