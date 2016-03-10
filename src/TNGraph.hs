module TNGraph 
	(TNGraph,
	 buildTNGraphFromInfo,
	 convertGraph,
	 getNeighbors,
	 getValue,
	 insertValue,
	 getGraphType,
	 addValuesToGraph,
	 divideGraphValuesBy
	) where

import qualified Data.Graph as G
import Data.Array
import TNTypes
import TNPrimitiveFunctions as TNP
import Data.List
import qualified Data.Map.Strict as M
import Control.Exception (assert)

-- | Build a TNGraph from its edges and vertex values.
buildTNGraphFromInfo:: TNGraphType -> TGraphInfo -> TNGraph
buildTNGraphFromInfo newGraphType graphInfo =
	TNGraph { table = graph , graphType = newGraphType :: TNGraphType, vertexValues = snd graphInfo}
	where
		edgesList = fst graphInfo
		flatEdges = concat (map (\(a,b) -> [a,b]) edgesList)
		bounds = if (length flatEdges == 0) then (0,0) else ((minimum flatEdges), (maximum flatEdges))
		graph = G.buildG bounds edgesList

-- | Convert a graph into another type.
-- Currently only implemented TNDGraph (directed) -> TNUGraph (undirected)
convertGraph:: TNGraph -> TNGraphType -> TNGraph
convertGraph (TNGraph {table = t, graphType = gt, vertexValues = values})  newGraphType =
	case [gt, newGraphType:: TNGraphType] of
		[TNDGraph, TNUGraph] -> directedGraphToUndirected (TNGraph {table = t, graphType = gt, vertexValues = values})

-- | Convert TNDGraph (directed) into TNUGraph (undirected)
-- For every edge (A -> B), this function adds the edge (B -> A),
-- effectively making an undirected graph.
directedGraphToUndirected:: TNGraph -> TNGraph
directedGraphToUndirected graph = buildTNGraphFromInfo TNUGraph (allEdges, values)
	where
		TNGraph {table = t, graphType = gt, vertexValues = values} = graph
		graphEdges = edges graph
		allEdges = concat $ map (\pair -> [pair, (snd pair, fst pair)]) (graphEdges)

-- | Get all neighbors for a vertex in a graph.
-- Surprisingly enough, this was not a primitive function previously implemented in Data.Graph
-- so I had to implement it myself.
getNeighbors:: TNGraph -> TNVertex -> [TNVertex]
getNeighbors (TNGraph {table = t, graphType = gt, vertexValues = values}) vId =
	if (vId >= fst b && vId <= snd b) then t ! vId else []
	where
		b = bounds t

-- | Get the value assigned to a vertex
getValue:: TNGraph -> TNVertex -> Maybe TNVertexValue
getValue (TNGraph {table = t, graphType = gt, vertexValues = values}) vId = M.lookup vId values

-- | Assign a value to a vertex
insertValue:: TNGraph -> TNVertex -> TNVertexValue -> TNGraph
insertValue (TNGraph {table = t, graphType = gt, vertexValues = values}) vId newVal =
	TNGraph {table = t, graphType = gt, vertexValues = newVals}
	where
		newVals = M.insert vId newVal values

-- | Get the graph's type (TNDGraph, or TNUGraph)
getGraphType:: TNGraph -> TNGraphType
getGraphType (TNGraph {table = t, graphType = gt, vertexValues = values}) = gt

-- |Adds a value to a graph's vertices
addValuesToGraph:: TNGraph -> TVertexValueMap -> TNGraph
addValuesToGraph graph valuesToAdd = TNGraph {table = t, graphType = gt, vertexValues = newValues}
	where
		(TNGraph {table = t, graphType = gt, vertexValues = values}) = graph
		newValues = M.unionWith (+) values valuesToAdd

-- |Divide graph values by a float
divideGraphValuesBy:: TNGraph -> Float -> TNGraph
divideGraphValuesBy graph denominator =
	TNGraph {table = t1, graphType = gt1, vertexValues = (M.fromList newValues)}
	where
		(TNGraph {table = t1, graphType = gt1, vertexValues = values1}) = graph
		newValues = if (denominator == 0) then M.toList values1 else map (\x -> (fst x, ((snd x) / denominator) :: Float)) (M.toList values1)

graph = buildTNGraphFromInfo TNDGraph ([(1, 2), (1, 3), (2, 4), (5, 7), (1,2),(1,2)], M.fromList [(1,1),(2,0.45),(3,1),(4,0.45),(5,0.45),(6,0.45),(7,0.45)])
