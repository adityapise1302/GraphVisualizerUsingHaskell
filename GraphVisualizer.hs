module GraphVisualizer where

import qualified Data.Map as Map

-- Adjacency List Representation
type Node = String
type Graph = Map.Map Node [Node]

emptyGraph :: Graph
emptyGraph = Map.empty

-- Add Edge
addEdge :: Graph -> Node -> Node -> Graph
addEdge graph from to = Map.insertWith (++) from [to] graph

-- Remove Edge
removeEdge :: Graph -> Node -> Node -> Graph
removeEdge graph from to = Map.adjust (filter (/= to)) from graph

-- Get Neighbors of a Node
getNeighbors :: Graph -> Node -> [Node]
getNeighbors graph node = Map.findWithDefault [] node graph

exampleGraph :: Graph
exampleGraph = addEdge (addEdge (addEdge emptyGraph "A" "B") "A" "C") "B" "D"

buildGraphFromEdges :: [(Node, Node)] -> Graph
buildGraphFromEdges edges = foldl (\g (from, to) -> addEdge g from to) emptyGraph edges

parseExampleGraph :: Graph
parseExampleGraph = buildGraphFromEdges [("A", "B"), ("A", "C"), ("B", "D")]

readGraphFromInput :: IO Graph
readGraphFromInput = do
  putStrLn "Enter edges in the format 'A B', one per line. Type 'done' to finish:"
  readEdges emptyGraph

readEdges :: Graph -> IO Graph
readEdges graph = do
  line <- getLine
  if line == "done"
    then return graph
    else do
      let (from, to) = parseEdge line
      readEdges (addEdge graph from to)

parseEdge :: String -> (Node, Node)
parseEdge line = let [from, to] = words line in (from, to)
