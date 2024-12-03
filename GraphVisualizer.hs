{-
    Graph Visualizer is a program which helps to visualize graph algorithms like 
    BFS, DFS, and Dijkstra Shortest Path. This is a useful tool to learn the graph
    algorithms.

    Team :- Aditya Pise, Clare O'Brien, Calvin Briscoe
    Class :- CSC372
-}
module GraphVisualizer where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)

-- Graph Representation: Adjacency List
type Node = (String,Int)
type Graph = Map.Map String [Node]

-- Create an empty graph
emptyGraph :: Graph
emptyGraph = Map.empty

-- Add an edge to the graph 
addEdge :: Graph -> String -> String -> Int -> Graph
addEdge graph from to dist =
  let graph' = Map.insertWith (++) from [(to, dist)] graph
  in Map.insertWith (++) to [(from, dist)] graph'
  
removeEdge :: Graph -> String -> String -> Graph
removeEdge graph from to =
    -- Get the name of each "to" node and remove if they don't match. Then do the same for from
  let graph' = Map.adjust (filter (\(toName, _) -> toName /= to)) from graph
      graph'' = Map.adjust (filter (\(fromName, _) -> fromName /= from)) to graph'
    -- If the neighbors list is empty for the to node, delete it from the graph
      graph''' = if null (Map.findWithDefault [] to graph'') then Map.delete to graph'' else graph''
  in graph'''

getNeighbors :: Graph -> String -> [Node]
getNeighbors graph node = Map.findWithDefault [] node graph

-- Return the first element in the tuple (the name) as a string
getNeighborNames :: Graph -> Node -> [String]
getNeighborNames graph (node,_) = map fst (Map.findWithDefault [] node graph)

-- BFS with Visualization
bfsWithVisualization :: Graph -> Node -> IO ()
bfsWithVisualization graph start = bfsHelper graph [start] [] 1

-- Helper function for BFS with Visualization
bfsHelper :: Graph -> [Node] -> [Node] -> Int -> IO ()
    -- If the queue is empty, return
bfsHelper _ [] visited step = do
  putStrLn $ "Step " ++ show step ++ ": BFS Complete!"
  putStrLn $ "Visited Nodes: " ++ show visited
    -- If non-empty queue, take the first node and check that it hasn't been visited
    -- If it has been visited, skip it
bfsHelper graph ((current,dist):queue) visited step
  | current `elem` map fst visited = do
      putStrLn $ "Step " ++ show step ++ ": Node '" ++ show current ++ "' already visited. Skipping."
      bfsHelper graph queue visited (step + 1)
    -- Otherwise, add the list of neighbors to the queue and add the current node to visited
  | otherwise = do
      let neighbors = getNeighborNames graph (current,dist)
      let newQueue = queue ++ [(n,dist) | n <- neighbors]
      let newVisited = visited ++ [(current,dist)]
      putStrLn $ "Step " ++ show step ++ ": Processing Node '" ++ show current ++ "'"
      putStrLn $ "  Queue: " ++ show newQueue
      putStrLn $ "  Visited: " ++ show newVisited
      bfsHelper graph newQueue newVisited (step + 1)

-- DFS with Visualization
dfsWithVisualization :: Graph -> Node -> IO ()
dfsWithVisualization graph start = dfsHelper graph [start] [] 1

-- Helper function for DFS with Visualization
dfsHelper :: Graph -> [Node] -> [Node] -> Int -> IO ()
    -- If the stack is empty, return
dfsHelper _ [] visited step = do
  putStrLn $ "Step " ++ show step ++ ": DFS Complete!"
  putStrLn $ "Visited Nodes: " ++ show visited
    -- If non-empty stack, pop the top and check that the current node hasn't been visited
    -- If it has been visited, skip it
dfsHelper graph ((current,dist):stack) visited step
  | current `elem` map fst visited = do
      putStrLn $ "Step " ++ show step ++ ": Node '" ++ show current ++ "' already visited. Skipping."
      dfsHelper graph stack visited (step + 1)
    -- Otherwise, add the current node to visited and push neighbors onto the stack
  | otherwise = do
      let newVisited = visited ++ [(current,dist)]
      let neighbors = getNeighborNames graph (current,dist)
      --let newStack = map fst neighbors ++ stack
      --let newStack = neighbors ++ stack
      let newStack = [(n, dist) | n <- neighbors] ++ stack
      putStrLn $ "Step " ++ show step ++ ": Processing Node '" ++ show current ++ "'"
      putStrLn $ "  Stack: " ++ show newStack
      putStrLn $ "  Visited: " ++ show newVisited
      dfsHelper graph newStack newVisited (step + 1)

-- Unweighted Dijkstra's Algorithm with Visualization
uwDijkstrasWithVisualization :: Graph -> Node -> IO ()
uwDijkstrasWithVisualization graph start = uwDijkstrasHelper graph [start] [] [] 1

-- Helper function for Unweighted Dijkstra's with Visualization
uwDijkstrasHelper :: Graph -> [Node] -> [Node] -> [Node] -> Int -> IO ()
uwDijkstrasHelper _ [] dists visited step = do
  putStrLn $ "Step " ++ show step ++ ": Unweighted Dijkstra's Complete!"
  putStrLn $ "Final Distances: " ++ show dists
    -- Get the current node from the beginning of the queue and check if it has been visited
    -- If the node has been visited, skip and find the shorter path
uwDijkstrasHelper graph ((current,dist):queue) dists visited step
  | current `elem` map fst visited = do
      putStrLn $ "Step " ++ show step ++ ": Node '" ++ show current ++ "' has shorter path. Skipping."
      uwDijkstrasHelper graph queue dists visited (step + 1)
    -- Otherwise, get the unvisited neighbors, update the distance to each neighbor, and sort by distance
  | otherwise = do
      let neighbors = getNeighborNames graph (current,dist)
      let unvisitedNeighbors = [n | n <- neighbors, not (n `elem` map fst visited)]
      let neighborDists = [(n, dist) | n <- unvisitedNeighbors]
      let newQueue = sortBy (compare `on` snd) (queue ++ neighborDists)
      let newDists = updateDistances dists ((current, dist) : neighborDists)
      putStrLn $ "Step " ++ show step ++ ": Processing Node '" ++ show current ++ "'"
      putStrLn $ "  Queue: " ++ show newQueue
      putStrLn $ "  Distances: " ++ show newDists
      uwDijkstrasHelper graph newQueue newDists ((current,dist):visited) (step+1)


-- loop helper function to print the right path (instead of just outputting visited)
loop :: [(String,String)] -> String -> String -> [String] -> [String]
loop visited start end res
    -- If start and end are the same add the end to the result and finish
    | end == start = res ++ [start]
    -- Else get the node that came before the current node (aka its neighbor) and call the loop again
    | otherwise = case lookup end visited of
        Just previous -> loop visited start previous (end:res)
        Nothing   -> error "Path does not exist!"

-- Check if a node is in the list of distances
checkDistances :: String -> [(String, Int)] -> Bool
checkDistances node distances = any (\(n, _) -> n == node) distances

-- Weighted Dijkstra's Algorithm with Visualization
dijkstrasWithVisualization :: Graph -> Node -> Node -> IO ()
dijkstrasWithVisualization graph start end = dijkstrasHelper graph [start] [] [] start end

-- Helper function for Weighted Dijkstra's Algorithm with Visualization
dijkstrasHelper :: Graph -> [Node] -> [Node] -> [(String, String)] -> Node -> Node -> IO ()
dijkstrasHelper _ [] _ _ _ _ = do putStrLn $ "All done!"
dijkstrasHelper graph ((current,dist):queue) distances visited start end
    -- When we hit the last node, we have to go back through and get the final result from visited
    | current == fst end = do
        let result = loop visited (fst start) (fst end) []
        putStrLn $ "Finished path: " ++ show result
        putStrLn $ "Total distance: " ++ show dist
    | otherwise = do
    -- Get neighbors list, find each of their distances from current, make sure they haven't been visited
    -- Update newVisit, go through unvisited and link next and current, then add visited to back of queue
        let neighbors = getNeighbors graph current
        let newUnvisited = [(next, d+dist) | (next, d) <- neighbors, not (checkDistances next distances)]
        let newVisited = [(next, current) | (next, _) <- newUnvisited] ++ visited
        let newDistances = updateDistances distances newUnvisited
        let newQueue = sortBy (compare `on` snd) (queue ++ newUnvisited)
        putStrLn $ "Processing Node '" ++ show current ++ "'"
        putStrLn $ "  Queue: " ++ show newQueue
        putStrLn $ "  Distances: " ++ show newDistances
        dijkstrasHelper graph newQueue newDistances newVisited start end

-- Updates the shortest distances for all nodes
updateDistances :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
updateDistances currentDists [] = currentDists
updateDistances currentDists (newDist:remainingDists) = 
    updateDistances (updateOne currentDists newDist) remainingDists

-- Updates the shortest distance for a single node
updateOne :: [(String, Int)] -> (String, Int) -> [(String, Int)]
updateOne [] (n, d) = [(n, d)]
updateOne ((n1, d1):ds) (n2, d2) =
    case n1 == n2 of
        True  -> if d1 < d2 then (n1, d1) : ds else (n2, d2) : ds
        False -> (n1, d1) : updateOne ds (n2, d2)

-- Parse an edge from a string like "A B 3"
parseEdge :: String -> (String, [(String, Int)])
parseEdge line = 
  let [from, to, dist] = words line
      distance = read dist :: Int  -- Convert dist to Int
  in (from, [(to, distance)])

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
  putStrLn "5. Run DFS"
  putStrLn "6. Run Unweighted Dijkstra's"
  putStrLn "7. Run Weighted Dijkstra's"
  putStrLn "8. Exit"
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
      runDFSCLI graph
      mainMenu graph
    "6" -> do
      runUWDijkstrasCLI graph
      mainMenu graph
    "7" -> do
      runDijkstrasCLI graph
      mainMenu graph
    "8" -> putStrLn "Goodbye!"
    _   -> do
      putStrLn "Invalid choice. Try again."
      mainMenu graph

-- Add Edge CLI
addEdgeCLI :: Graph -> IO Graph
addEdgeCLI graph = do
  putStrLn "Add an edge (format: FROM TO DISTANCE):"
  edge <- getLine
  let (from, [(to, dist)]) = parseEdge edge
  let graph' = addEdge graph from to dist
  putStrLn $ "Edge added: " ++ from ++ " <-> " ++ to ++ ", " ++ show dist
  return graph'

-- Remove Edge ClI
removeEdgeCLI :: Graph -> IO Graph
removeEdgeCLI graph = do
  putStrLn "Remove an edge (format: FROM TO DISTANCE):"
  edge <- getLine
  let (from, [(to, dist)]) = parseEdge edge
    -- Send only the names of the nodes to be removed to removeEdge
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
    then bfsWithVisualization graph (start,0)
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

-- Run Dijkstras CLI
runUWDijkstrasCLI :: Graph -> IO()
runUWDijkstrasCLI graph = do
  putStrLn "Enter the starting node for unweighted Dijkstras"
  start <- getLine
  if Map.member start graph
    then uwDijkstrasWithVisualization graph (start,0)
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

runDijkstrasCLI :: Graph -> IO()
runDijkstrasCLI graph = do
  putStrLn "Enter the starting node for weighted Dijkstras"
  start <- getLine
  putStrLn "Enter the ending node for weighted Dijkstras"
  end <- getLine
  if Map.member start graph
    then if Map.member end graph
      then dijkstrasWithVisualization graph (start,0) (end,0)
      else putStrLn $ "Node '" ++ end ++ "' is not connected to the graph."
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

runDFSCLI :: Graph -> IO()
runDFSCLI graph = do
  putStrLn "Enter the starting node for DFS:"
  start <- getLine
  if Map.member start graph
    then dfsWithVisualization graph (start,0)
    else putStrLn $ "Node '" ++ start ++ "' is not connected to the graph."

-- Main Function
main :: IO ()
main = cli
