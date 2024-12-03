{-
    Tests for GraphVisulalizer helper functions. Since the program uses IO and user input test cases are not really practical as 
    you cannot find differences without a bash script. To Test these functions you can load into the program and add some edges
    and call the visulization functions.
    INSTRUCTIONS TO TEST:
    DOWNLOAD GHCup HERE: https://www.haskell.org/ghcup/
    Then in the Project file run the command
    ghci - to get into the interactive shell
    from there compile both files using 
    -l Tests.hs or (-l GraphVisualizer.hs) to load the file
    then from there run the main function
    -=-
    for Test.hs run tests
    for GraphVisualizer.hs run main 
-}
module Tests where

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import GraphVisualizer

-- Helper function to print test results
testIO :: (Show a, Eq a) => String -> a -> a -> IO ()
testIO name result expected = do
    putStrLn $ name ++ ": ..." ++ if result == expected then " PASSED!" else " FAILED!"

-- Test add an Edge to empty unweighted graph
testAddEdge1 :: IO()
testAddEdge1 = do
    let graph = addEdge emptyGraph "A" "B" 1
    let result = Map.fromList[("A",[("B",1)]),("B",[("A",1)])]
    testIO  "testAddEdge1" graph result

-- Test to remove Edges
testAddMulitple :: IO()
testAddMulitple = do
    let graph = addEdge emptyGraph "A" "B" 1
    let graph' = addEdge graph "A" "C" 10
    let result = Map.fromList[("A",[("C", 10), ("B",1)]),("B",[("A",1)]), ("C",[("A",10)])]
    testIO  "testAddEdge1" graph' result

    -- Test to remove an edge
testRemove1 :: IO ()
testRemove1 = do
    let graph = addEdge emptyGraph "A" "B" 1
    let graph' = addEdge graph "A" "C" 10
    let graph'' = removeEdge graph' "A" "C"
    let result = Map.fromList[("A",[("B",1)]),("B",[("A",1)])]
    testIO  "testRemove1" graph'' result

-- try to remove an edge that doesn't exist... nothing should happen
testRemoveNoEdge :: IO ()
testRemoveNoEdge = do
    let graph = addEdge emptyGraph "A" "B" 1
    let graph' = addEdge graph "A" "C" 10
    let graph'' = removeEdge graph' "B" "C"
    let result = Map.fromList[("A",[("B",1)]),("B",[("A",1)])]
    testIO  "testRemoveNoEdge" graph'' graph'

-- Test to remove an edge
testNeighbors :: IO ()
testNeighbors = do
    let graph = addEdge emptyGraph "A" "B" 1
    let graph' = addEdge graph "A" "C" 10
    let graph'' = addEdge graph' "A" "D" 1
    let neightbor = getNeighbors graph'' "A"
    let result = [("D", 1),("C", 10),("B", 1)]
    testIO  "testNeighbors" neightbor result

-- Test to remove an edge
testNoNeighbors :: IO ()
testNoNeighbors = do
    let graph = addEdge emptyGraph "A" "B" 1
    let graph' = addEdge graph "A" "C" 10
    let graph'' = addEdge graph' "A" "D" 1
    let graph''' = removeEdge graph'' "A" "D"
    let neightbor = getNeighbors graph''' "D"
    let result = []
    testIO  "testNoNeighbors" neightbor result

--Test update function
testUpdates :: IO ()
testUpdates = do
    let neighbors = [("A", 10), ("B", 5)]
    let newNeighbors = [("A", 1), ("C", 1)]
    let updatedDists = updateDistances neighbors newNeighbors
    let result = [("A", 1), ("B", 5), ("C", 1)]
    testIO  "testUpdates" updatedDists result

--Test parse edge
testParseEdge :: IO ()
testParseEdge = do
    let parsed = parseEdge "A B 1"
    let result = ("A", [("B", 1)])
    testIO "testParseEdge" parsed result

-- main function to run Tests
tests :: IO ()
tests = do
    putStrLn "Running tests..."
    testAddEdge1
    testAddMulitple
    testRemove1
    testRemoveNoEdge
    testNeighbors
    testNoNeighbors
    testUpdates
    testParseEdge
    putStrLn $ "...END TESTS."
