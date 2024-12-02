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

-- main function to run Tests
tests :: IO ()
tests = do
    putStrLn "Running tests..."
    testAddEdge1
    testAddMulitple
