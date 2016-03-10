module Main (main) where

import Test.QuickCheck
import Test.Hspec
import TNGraph
import Data.Map as M
import TNTypes
import TNPrimitiveFunctions
import Data.Maybe (fromMaybe)
import Test.QuickCheck

main :: IO ()

main = hspec $ describe "Testing TNGraph2.0" $ do

    describe "buildTNGraphFromInfo" $
      it "Edges = [(1,2)] -> Vertices = [1,2]" $ do
      	let edges = [(1,2)]
      	let vertexValues = M.fromList [(1,3),(2,4)]
      	let graphInfo = (edges, vertexValues)
      	let graph = buildTNGraphFromInfo TNDGraph graphInfo
    	let vs = vertices graph
        vs `shouldBe` [1,2]