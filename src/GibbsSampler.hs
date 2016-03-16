module GibbsSampler
	(getPosteriorSampling,
	 randomInitializer,
	 zerosInitializer
	) where

import TNGraph
import TNPrimitiveFunctions
import System.Random
import qualified Data.Map as M
import TNTypes
import TNParser
import Data.Maybe
import Debug.Trace

-- **** GibbsSampler ****
-- This file contains all the functions that utilize TNGraph
-- to do image denoising.

-- Y is the graph of unobserved, real image pixel values
-- X is the graph of observed, noisy pixel values

-- **** Types ****
-- |In the case of an undirected graph, the Markov Blanket for a vertex
-- is the set of all of its neighbors.
type MarkovBlanket = ([TNVertexValue], TNVertexValue)

-- |Return the Markov Blanket for a given vertex
markovBlanket:: TNVertex -> TNGraph -> TNGraph -> MarkovBlanket
markovBlanket vId yGraph xGraph = (yNeighborVals, fromJust $ xNeighborVal)
	where
		yNeighbors = getNeighbors yGraph vId
		yNeighborVals = map (\n -> fromJust $ getValue yGraph n) yNeighbors
		xNeighborVal = getValue xGraph vId

-- **** Y graph initializers **** (which represents unobserved, real pixel values)
-- Different initializations can ultimately yield different denoised image outputs

-- |Return Y, initialized randomly
randomInitializer:: TNGraph -> StdGen -> TNGraph
randomInitializer xGraph randGen = 
	TNGraph { table = t , graphType = gt, vertexValues = M.fromList newVals}
	where
		(TNGraph {table = t, graphType = gt, vertexValues = values}) = xGraph
		randVals = take (length $ vertices xGraph) (randoms (randGen) :: [Float])
		newVals = zip (vertices xGraph) (map (\x -> if x < 0.5 then -1 else 1) randVals)

-- |Return Y, a copy of X with all values initialized to zeros
zerosInitializer:: TNGraph -> TNGraph
zerosInitializer graph = 
	TNGraph { table = t , graphType = gt, vertexValues = M.fromList newVals}
	where
		(TNGraph {table = t, graphType = gt, vertexValues = values}) = graph
		vs = vertices graph
		newZeroVals = map (\x ->  0) [1.. length(vs)]
		newVals = zip vs newZeroVals

-- **** Sampling ****

-- |Returns the probability that the vertex is black,
-- givent vertex's Markov Blanket
sampleProb:: MarkovBlanket -> Float
sampleProb blanket = 1 / denominator
	where
		(nu, beta) = (1, 1)
		(yNeighbors, xNeighbor) = blanket
		first_sum = nu * (foldl (+) 0 yNeighbors)
		second_sum = beta * xNeighbor
		denominator = 1 + exp (-2 * first_sum - 2 * second_sum)

-- |Samples the value for a vertex (either 1 or -1).
-- It first estimates the probablity that the vertex is black using sample_prob, 
-- and checks if a randomly drawn decimal (0,1) falls within that probability.
sample:: TNVertex -> TNGraph -> TNGraph -> StdGen -> (TNVertexValue, StdGen)
sample vId yGraph xGraph randGen = 
	if (randNum < prob) then (1,newRandGen)  else (-1, newRandGen)
	where
		(yNeighbors, xNeighbor) = markovBlanket vId yGraph xGraph
		prob = sampleProb (markovBlanket vId yGraph xGraph)
		(randNum, newRandGen) = (random randGen :: (Float, StdGen))
	--if (randNum < prob) then (1,newRandGen)  else (-1, newRandGen)

-- |Gathers the samples for all vertices in a graph.
gatherSamples:: TNGraph -> TNGraph -> StdGen -> [TNVertex] -> [TNVertexValue] -> [TNVertexValue]
gatherSamples yGraph xGraph randGen [] currResult = currResult

gatherSamples yGraph xGraph randGen vsLeft currResult = 
	gatherSamples yGraph xGraph newRandGen (tail vsLeft) newResult
	where
		currVertex = head vsLeft
		(sampleValue, newRandGen) = sample currVertex yGraph xGraph randGen
		newResult = currResult ++ [sampleValue]

-- |Returns a Gibbs sample, given Y and X.
gibbsSample:: TNGraph -> TNGraph -> StdGen -> TNGraph
gibbsSample yGraph xGraph randGen = 
	TNGraph {table = t, graphType = gt, vertexValues = sampleVertexVals}
	where
		newSampleVals = gatherSamples yGraph xGraph randGen (vertices yGraph) []
		TNGraph {table = t, graphType = gt, vertexValues = newVals} = yGraph
		sampleVertexVals = M.fromList (zip (vertices yGraph) newSampleVals)

-- |Adds a value to a graph's vertices
addOnesFromSample:: TNGraph -> TVertexValueMap -> TNGraph
addOnesFromSample prevCountsGraph newValues = 
	addValuesToGraph prevCountsGraph ones
	where ones = M.map (\x -> if (x == 1) then 1 else 0) newValues

-- |Get posterior probability for all vertices in Y
getPosteriorSampling:: TNGraph -> TNGraph -> Int -> Int -> StdGen -> TNGraph
getPosteriorSampling xGraph yGraph numSamples numBurns randGen =
	getPosteriorSamplingRecursive xGraph yGraph numSamples numBurns 0 (zerosInitializer xGraph) randGen

-- |Recursive function to calculate Gibbs samples. It's tail recursive.
getPosteriorSamplingRecursive:: TNGraph -> TNGraph -> Int -> Int -> Int -> TNGraph -> StdGen -> TNGraph
getPosteriorSamplingRecursive xGraph yGraph numSamples numBurns itNum posterior randGen = do
	if (itNum >= numSamples)
	then 
		divideGraphValuesBy posterior (fromIntegral numSamples)
		else 
			getPosteriorSamplingRecursive xGraph newSample numSamples numBurns (itNum + 1) newPosterior newRandGen
			where
				(randNum, newRandGen) = (random randGen :: (Float, StdGen))
				newSample = gibbsSample xGraph yGraph newRandGen
				TNGraph {table = samplet, graphType = gtsample, vertexValues = sampleVals} = newSample
				newPosterior = addOnesFromSample posterior sampleVals

-- |Returns the TNVertex that lives at the given coordinates, given some graph dimensions
coordinatesToVertexId:: (Int, Int) -> (Int, Int) -> Maybe TNVertex
coordinatesToVertexId coordinates graphShape =
	if ((fst coordinates >= fst graphShape ||  fst coordinates < 0) 
		|| (snd coordinates >= snd graphShape ||  snd coordinates < 0)) then
		Nothing
		else Just $ (fst coordinates) * numVerticesPerLine +  (snd coordinates)
			where
				numVerticesPerLine = (snd graphShape)

-- |Returns the Coordinates for the given TNVertex, given some graph dimensions
vertexIdToCoordinates:: TNVertex -> (Int, Int) -> Maybe (Int, Int)
vertexIdToCoordinates vId graphShape =
	if (vId < 0 || vId >= (fst graphShape) * (snd graphShape)) 
		then Nothing
		else Just $ (numLinesBefore, colNum)
			where
				numVerticesPerLine = (snd graphShape)
				numLinesBefore = div vId numVerticesPerLine
				numVerticesBefore = numLinesBefore * numVerticesPerLine
				colNum = vId - numVerticesBefore
