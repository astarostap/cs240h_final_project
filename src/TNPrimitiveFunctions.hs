module TNPrimitiveFunctions (
	 vertices,
	 edges,
	 outdegree,
	 topsort,
	 reachable,
	 path,
	 components,
	 scComponents,
	 bcComponents
	) where

import qualified Data.Graph as G
import TNTypes

-- **** WRAPPER for native functions in Data.Graph ****
-- This allows us to call Data.Graph functions on our TNGraph.
-- All documentation for these functions can be found here: 
-- https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Graph.html

vertices :: TNGraph -> [TNVertex]
vertices (TNGraph {table = tb}) = G.vertices tb

edges :: TNGraph -> [TNEdge]
edges (TNGraph {table = tb}) = G.edges tb

outdegree :: TNGraph -> TNTable Int
outdegree (TNGraph {table = tb}) = G.outdegree tb

indegree :: TNGraph -> TNTable Int
indegree (TNGraph {table = tb}) = G.indegree tb

topsort :: TNGraph -> [TNVertex]
topsort (TNGraph {table = tb}) = G.topSort tb

reachable :: TNGraph -> TNVertex -> [TNVertex]
reachable (TNGraph {table = tb}) vertex = G.reachable tb vertex

path :: TNGraph -> TNVertex -> TNVertex -> Bool
path (TNGraph {table = tb}) start dest = G.path tb start dest

components :: TNGraph -> G.Forest TNVertex
components (TNGraph {table = tb}) = G.components tb

scComponents :: TNGraph -> G.Forest TNVertex
scComponents (TNGraph {table = tb}) = G.scc tb

bcComponents :: TNGraph -> G.Forest [TNVertex]
bcComponents (TNGraph {table = tb}) = G.bcc tb
