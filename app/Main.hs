module Main where

import TNTypes
import System.IO
import TNGraph as TNGraph
import TNParser
import GibbsSampler
import qualified Data.Map as M

import System.Environment
import System.Exit
import System.Random

main :: IO ()
main = do
    args <- getArgs
    randGen <- getStdGen
    case args of
    	["gibbs", string, int] -> performGibbsSampling string (read int :: Int) randGen
    	_ -> putStrLn "Usage: snap-haskell-exe gibbs <input-file> <num-samples>"	

-- |Reads a file and performs Gibbs sampling with the given number of samples.
-- Outputs a tab-delimited list where each row is of the form: TNVertex TNVertexValue
performGibbsSampling :: FilePath -> Int -> StdGen -> IO()
performGibbsSampling file numSamples randGen = readFile file >>= check . parseTNGraphEdgesFile
  where check (Right xGraph) = do
  			let posteriorProbabilities = getPosteriorSampling xGraph (zerosInitializer xGraph) numSamples 0 randGen
  			let (TNGraph {table = t1, graphType = gt1, vertexValues = values}) = posteriorProbabilities
  			let vals = map (\x -> (show $ fst x) ++ "\t" ++ (show $ snd x) ++ "\n") (M.toList values)
  			putStrLn $ (concat vals)
        check (Left msg) = fail $ file ++ ": " ++ msg
