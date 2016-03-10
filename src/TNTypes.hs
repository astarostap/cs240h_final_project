module TNTypes (
	TNVertex,
	TNEdge,
	TNTableBounds,
	TNTable,
	TNGraph(..),
	TNGraphType(..),
	TGraphInfo,
	TNVertexValue,
	TVertexValueMap
	) where

import qualified Data.Graph as G
import qualified Data.Map.Strict as M

-- **** TYPES ****
-- These are the types used for TNGraph.

-- |A @TNVertex@ is a @Data.Graph.Vertex@.
-- It represents the id for a vertex in the TNGraph.
type TNVertex = G.Vertex

-- |A @TNVertexValue@ is a Float value assigned to a TNVertex.
type TNVertexValue = Float

-- |A @TNEdge@ is a @Data.Graph.Edge@.
-- It represents an edge between two @TNVertex@s
type TNEdge = G.Edge

-- |@TNTableBounds@s are the bounds for the adjacy table @TNTable@.
type TNTableBounds = G.Bounds


-- |@TNTable@ is the adjacency table for @TNGraph@
type TNTable a = G.Table a

-- |@TVertexValueMap@ holds the values assigned to head vertex.
-- It is a map from @TNVertex@s to @TNVertexValue@s 
type TVertexValueMap = M.Map TNVertex TNVertexValue

-- |@TGraphInfo@ holds all the information we need to construct a TNGraph.
-- Some vertices might not have values assigned. This means that there might be
-- some vertices in [TNEdge] that do not have a value assigned in @TVertexValueMap@.
type TGraphInfo = ([TNEdge], TVertexValueMap)

-- |@TNGraphType@ holds all the possible types of graph.
-- @TNDGraph@: directed graph 
-- @TNUGraph@: undirected graph (for each edge going out a vertex, there is another edge coming into it).
data TNGraphType = TNUGraph | TNDGraph deriving (Eq, Ord, Show)

-- |@TNGraph@ is the TNGraph data structure
-- It stands for "Type Network Graph"
data TNGraph =  TNGraph {
					table :: TNTable [TNVertex],
					graphType :: TNGraphType,
					vertexValues :: TVertexValueMap
				} deriving (Show)




