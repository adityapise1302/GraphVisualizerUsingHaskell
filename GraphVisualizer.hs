module GraphVisualizer where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)

-- Graph Representation: Adjacency List
type Node = String
type Graph = Map.Map Node [Node]


-- Create an empty graph
emptyGraph :: Graph
emptyGraph = Map.empty

-- Add an edge to the graph 
addEdge :: Graph -> Node -> Node -> Graph
addEdge graph from to =
  let graph' = Map.insertWith (++) from [to] graph
  in Map.insertWith (++) to [from] graph'

-- Remove an edge 
removeEdge :: Graph -> Node -> Node -> Graph
removeEdge graph from to =
  let graph' = Map.adjust (filter (/= to)) from graph
  in Map.adjust (filter (/= from)) to graph'

-- Get neighbors of a node
getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = Map.findWithDefault [] node graph

-- BFS with Visualization
bfsWithVisualization :: Graph -> Node -> IO ()
bfsWithVisualization graph start = bfsHelper graph [start] [] 1

-- Helper function for BFS with Visualization
bfsHelper :: Graph -> [Node] -> [Node] -> Int -> IO ()
bfsHelper _ [] visited step = do
  putStrLn $ "Step " ++ show step ++ ": BFS Complete!"
  putStrLn $ "Visited Nodes: " ++ show visited
bfsHelper graph (current:queue) visited step
  | current `elem` visited = do
      putStrLn $ "Step " ++ show step ++ ": Node '" ++ current ++ "' already visited. Skipping."
      bfsHelper graph queue visited (step + 1)
  | otherwise = do
      let neighbors = getNeighbors graph current
      let newQueue = queue ++ neighbors
      let newVisited = visited ++ [current]
      putStrLn $ "Step " ++ show step ++ ": Processing Node '" ++ current ++ "'"
      putStrLn $ "  Queue: " ++ show newQueue
      putStrLn $ "  Visited: " ++ show newVisited
      bfsHelper graph newQueue newVisited (step + 1)

-- Dijkstra's Algorithm with Visualization
dijkstrasWithVisualization :: Graph -> Node -> IO ()
dijkstrasWithVisualization graph start = dijkstrasHelper graph [(start, 0)] [] [] 1

-- Helper function for Dijkstra's with Visualization
dijkstrasHelper :: Graph -> [(Node, Int)] -> [(Node, Int)] -> [Node] -> Int -> IO ()
dijkstrasHelper _ [] dists visited step = do
  putStrLn $ "Step " ++ show step ++ ": Dijkstra's Complete!"
  putStrLn $ "Final Distances: " ++ show dists
dijkstrasHelper graph ((current,dist):queue) dists visited step
  | current `elem` visited = do
      putStrLn $ "Step " ++ show step ++ ": Node '" ++ current ++ "' has shorter path. Skipping."
      dijkstrasHelper graph queue dists visited (step + 1)
  | otherwise = do
      let neighbors = getNeighbors graph current
      let unvisitedNeighbors = [n | n <- neighbors, not(n `elem` visited)]
      let neighborDists = [(n, dist + 1) | n <- unvisitedNeighbors]
      let newQueue = sortBy (compare `on` snd) (queue ++ neighborDists)
      let newDists = updateDistances dists ((current, dist) : neighborDists)
      putStrLn $ "Step " ++ show step ++ ": Processing Node '" ++ current ++ "'"
      putStrLn $ "  Queue: " ++ show newQueue
      putStrLn $ "  Distances: " ++ show newDists
      dijkstrasHelper graph newQueue newDists (current:visited) (step + 1)

-- Updates the shortest distances for all nodes
updateDistances :: [(Node, Int)] -> [(Node, Int)] -> [(Node, Int)]
updateDistances currentDists [] = currentDists
updateDistances currentDists (newDist:remainingDists) = 
    updateDistances (updateOne currentDists newDist) remainingDists

-- Updates the shortest distance for a single node
updateOne :: [(Node, Int)] -> (Node, Int) -> [(Node, Int)]
updateOne [] (n, d) = [(n, d)]
updateOne ((n1, d1):ds) (n2, d2) =
    case n1 == n2 of
        True  -> if d1 < d2 then (n1, d1) : ds else (n2, d2) : ds
        False -> (n1, d1) : updateOne ds (n2, d2)

-- Parse an edge from a string like "A B"
parseEdge :: String -> (Node, Node)
parseEdge line = let [from, to] = words line in (from, to)

-- Command-Line Interface for Graph Visualizer
cli :: IO ()
cli = do
  putStrLn "Welcome to the Graph Visualizer!"
  mainMenu emptyGraph

-- Main Menu
mainMenu :: Graph -> IO ()
mainMenu graph = do
  putStrLn "\nMain Menu:"
  putStrLn "1. Add Edge"
  putStrLn "2. Remove Edge"
  putStrLn "3. Show Graph"
  putStrLn "4. Run BFS"
  putStrLn "5. Run Dijkstra's"
  putStrLn "6. Exit"
  putStr "Enter your choice: "
  choice <- getLine
  case choice of
    "1" -> do
      graph' <- addEdgeCLI graph
      mainMenu graph'
    "2" -> do
      graph' <- removeEdgeCLI graph
      mainMenu graph'
    "3" -> do
      putStrLn "Current Graph:"
      print graph
      mainMenu graph
    "4" -> do
      runBFSCLI graph
      mainMenu graph
    "5" -> do
      runDijkstrasCLI graph
      mainMenu graph
    "6" -> putStrLn "Goodbye!"
    _   -> do
      putStrLn "Invalid choice. Try again."
      mainMenu graph

-- Add Edge CLI
addEdgeCLI :: Graph -> IO Graph
addEdgeCLI graph = do
  putStrLn "Add an edge (format: FROM TO):"
  edge <- getLine
  let (from, to) = parseEdge edge
  let graph' = addEdge graph from to
  putStrLn $ "Edge added: " ++ from ++ " <-> " ++ to
  return graph'

-- Remove Edge ClI
removeEdgeCLI :: Graph -> IO Graph
removeEdgeCLI graph = do
  putStrLn "Remove an edge (format: FROM TO):"
  edge <- getLine
  let (from, to) = parseEdge edge
  let graph' = removeEdge graph from to
  case graph' == graph of
    True -> do
      putStrLn "Edge not found!"
      return graph
    False -> do
      putStrLn $ "Edge removed: " ++ from ++ " <-> " ++ to
      return graph'
      
-- Run BFS CLI
runBFSCLI :: Graph -> IO ()
runBFSCLI graph = do
  putStrLn "Enter the starting node for BFS:"
  start <- getLine
  if Map.member start graph
    then bfsWithVisualization graph start
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

-- Run Dijkstras CLI
runDijkstrasCLI :: Graph -> IO()
runDijkstrasCLI graph = do
  putStrLn "Enter the starting node for Dijkstras"
  start <- getLine
  if Map.member start graph
    then dijkstrasWithVisualization graph start
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

-- Main Function
main :: IO ()
main = cli
