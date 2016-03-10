

module Main (main) where

import Test.QuickCheck
import Test.Hspec
import TNGraph
import qualified Data.Map as M
import TNTypes
import TNPrimitiveFunctions
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Data.List
import GibbsSampler
import System.Random

instance Arbitrary TNGraphType where
  arbitrary     = elements [TNDGraph, TNUGraph]

main :: IO ()

main = hspec $ describe "Testing" $ do

    describe "TNGraph.buildTNGraphFromInfo" $
      it "Edges = [(1,2)] -> Vertices = [1,2]" $ do
      	let edges = [(1,2)]
      	let vertexValues = M.fromList [(1,3),(2,4)]
      	let graphInfo = (edges, vertexValues)
      	let graph = buildTNGraphFromInfo TNDGraph graphInfo
    	let vs = vertices graph
        vs `shouldBe` [1,2]

    describe "TNGraph.buildTNGraphFromInfo" $
      it "Basic graph properties" $ do
      	let edges = [(1,2)]
      	let vertexValues = M.fromList [(1,3),(2,4)]
      	let graphInfo = (edges, vertexValues)
      	let (TNGraph {table = t, graphType = gt, vertexValues = values})
      		 = buildTNGraphFromInfo TNDGraph graphInfo
        gt `shouldBe` TNDGraph
        values `shouldBe` M.fromList [(1,3),(2,4)]

    describe "TNGraph.convertGraph: directed -> undirected" $
      it "must double the number of edges" $ property prop_convertGraphDoublesEdges

    describe "GibbsSampler.divideGraphValuesBy" $
      it "new values must equal old values times denominator" $ property prop_divideGraphValuesBy

    describe "GibbsSampler.addGraphValues" $
      it "new values must equal sum of previous values plus sum of added values" $ property prop_addGraphValues

    describe "GibbsSampler.randomInitializer" $
      it "must not change graph dimensions" $ property prop_randomInitializer

    describe "GibbsSampler.zerosInitializer" $
      it "must not change graph dimensions" $ property prop_zerosInitializerDoesNotChangeDimensions

    describe "GibbsSampler.zerosInitializer" $
      it "all vertex values must add up to zero" $ property prop_zerosInitializerHasZeros

    describe "TNGraph.getGraphType" $
      it "must return graph type with which graph was built" $ property prop_getGraphType

    describe "TNGraph.convertGraph" $
      it "must update graph type to new type" $ property prop_convertGraphToUndirectedUpdatesType

-- | Converting a directed graph into an undirected graph should 
-- double the number of edges in the graph.
prop_convertGraphDoublesEdges:: ([TNEdge], [(Int, Float)]) -> Bool
prop_convertGraphDoublesEdges graphInfo = do
	let (ed, vertexVals) = graphInfo
	let directedGraph = buildTNGraphFromInfo TNDGraph (ed, M.fromList vertexVals)
	let undirectedGraph = convertGraph directedGraph TNUGraph
	let newEdges = edges undirectedGraph
	length (newEdges) == 2 * length (ed)

prop_convertGraphToUndirectedUpdatesType:: ([TNEdge], [(Int, Float)]) -> Bool
prop_convertGraphToUndirectedUpdatesType graphInfo = do
  let (ed, vertexVals) = graphInfo
  let directedGraph = buildTNGraphFromInfo TNDGraph (ed, M.fromList vertexVals)
  let undirectedGraph = convertGraph directedGraph TNUGraph
  getGraphType undirectedGraph == TNUGraph

prop_divideGraphValuesBy:: ([TNEdge], [(Int, Float)]) -> Float -> Bool
prop_divideGraphValuesBy graphInfo denominator = do
  let (ed, vertexVals) = graphInfo
  let graph = buildGraph graphInfo
  let TNGraph {table = t, graphType = gt, vertexValues = newVals} = divideGraphValuesBy graph denominator
  let add = M.foldl (+) 0
  (add $ M.fromList vertexVals) - (add newVals) * denominator <= 0.1

prop_addGraphValues:: ([TNEdge], [(Int, Float)]) -> [(Int, Float)] -> Bool
prop_addGraphValues graphInfo valuesToAdd = do
  let (ed, vertexVals) = graphInfo
  let graph = buildGraph graphInfo
  let TNGraph {table = t, graphType = gt, vertexValues = newVals} = addValuesToGraph graph (M.fromList valuesToAdd)
  let add = M.foldl (+) 0
  (add $ M.fromList vertexVals) + (add $ (M.fromList valuesToAdd)) - add newVals <= 0.2

prop_getGraphType:: ([TNEdge], [(Int, Float)]) -> TNGraphType -> Bool
prop_getGraphType graphInfo graphType = do
  let (ed, vertexVals) = graphInfo
  let graph = buildTNGraphFromInfo graphType (ed, M.fromList vertexVals)
  getGraphType graph == graphType

prop_randomInitializer::([TNEdge], [(Int, Float)]) -> Bool
prop_randomInitializer graphInfo = do
  length (vertices $ randomInitializer (buildGraph graphInfo) (mkStdGen 1)) == length (vertices (buildGraph graphInfo))

prop_zerosInitializerDoesNotChangeDimensions::([TNEdge], [(Int, Float)]) -> Bool
prop_zerosInitializerDoesNotChangeDimensions graphInfo = do
  length (vertices $ zerosInitializer (buildGraph graphInfo)) == length (vertices (buildGraph graphInfo))

prop_zerosInitializerHasZeros::([TNEdge], [(Int, Float)]) -> Bool
prop_zerosInitializerHasZeros graphInfo = do
  let TNGraph {table = t, graphType = gt, vertexValues = newVals} = zerosInitializer (buildGraph graphInfo)
  let add = M.foldl (+) 0
  add newVals == 0

buildGraph::([TNEdge], [(Int, Float)]) -> TNGraph
buildGraph graphInfo = buildTNGraphFromInfo TNUGraph (ed, M.fromList vertexVals)
  where 
    (ed, vertexVals) = graphInfo
