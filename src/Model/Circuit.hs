{-# LANGUAGE GADTs #-}
module Model.Circuit where
{--
A prototype representation of connectable gate components

As is, this implementation can't interface directly with a UI, because inputs
could change as the user updates the sandbox. Some use of monads is likely
needed.

Below is an example of an adder implemented with these gates.
https://media.geeksforgeeks.org/wp-content/uploads/3-57.png
-}
{-#LANGUAGE arrows #-}
{-# LANGUAGE RankNTypes #-}
import FRP.Yampa
import Data.Set
import Data.HashMap.Strict
import Algebra.Graph
import Data.Set.Monad
--import Data.Lens


--first number corresponds to the gate number, the second corresponds to the gate input number
--type InputSet = HashMap
type Input  = (Set (Int, Int))
data Output = O Int (SF a Bool)
data Node = N Int (Bool->Bool-> Bool) (forall a. SF a Bool)
type NodeMap = NM AdjacencyIntMap (HashMap.Strict Int Node)

emNG :: NodeMap
emNG = empty

data Circuit where
  C :: Set Input -> AdjacencyMap Node -> Set Output -> Circuit

recalculate :: AdjacencyIntMap -> NodeMap -> NodeMap

instance Ord Node where
  (<=) (N i _ _) (N i' _ _) = i <= i'

instance Graph (Circuit) where
    type Vertex (Circuit) = Node
    empty = (C (EP empty) empty empty (EP -1 (constant false)))

    vertex (i, Node f stream) = C (fromList [(singleton (i,0)), singleton (i, 1) ]) 
                                  (vertex i)
                                  (insert i node empt)
                                  (O i (arr f))
    overlay (C si aim nm so) (C si' aim' nm' so') = C (mergeInputs si si') (aim + aim') (union so so')
      where
        mergeInputs si si' = fromList (u ++ map (\s -> [a | a <- s, not (isSubsetOf a (join u))]) [si, si'])
          where 
            u = fromList [union a b | a <- si, b <- si', not $ disjoint a b]
        graphCheck aim'' = (gmap (\n -> if member n diff then (recLoop n) else n) loops)
        --overlay <function turning new diff set into graph> (map recLoop diff) (scc aim) 
          where
            loops = topSort . scc aim''
            diff = (difference (vertexSet . loops) (vertexSet . scc aim))
            -- <edge relation of merged graph with which to reconstruct merged graph with updated nodes>

        recLoop g aim nodeSet = 
          where
            s = vertexSet g



        updateNodes aim'' = (do
          n <- (vertexSet aim)
          diffn <-difference (preset n aim'') (preset n aim)
          return diffn) 
          where
            list = vertexList 

        
        
          
    connect
