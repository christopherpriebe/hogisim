{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

--import Data.HashMap.Strict
--import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm
import Data.Set.Monad
import Algebra.Graph.Class
import Control.Monad
import Control.Applicative (Applicative(liftA2))
import Data.List
import qualified Data.List.NonEmpty
import FRP.Yampa.Arrow (arr2)
--import Data.Lens


--first number corresponds to the gate number, the second corresponds to the gate input number
--type InputSet = HashMap
type Input  = (Set (Int, Int))
data Output = O Int (forall a. SF a Bool)
data Node = N Int (forall a. a-> Bool) (forall a. SF a Bool)
--type NodeMap = NM AdjacencyMap (HashMap.Strict Int Node)

--emNG :: NodeMap
--emNG = empty

data Circuit where
  C :: Set Input -> AdjacencyMap Node -> Set Output -> Circuit

--recalculate :: AdjacencyIntMap -> NodeMap -> NodeMap

instance Ord Node where
  (<=) (N i _ _) (N i' _ _) = i <= i'

instance Graph Circuit where
    type Vertex Circuit = Node
    --empty = C (EP empty) empty empty (EP -1 (constant false))

    vertex (N i f s) = C (fromList [singleton (i,0), singleton (i, 1) ])
                                  (Algebra.Graph.AdjacencyMap.vertex (N i f (arr f)))
                                  (singleton (O i (arr f)))
    overlay (C si aim so) (C si' aim'  so') = C (mergeInputs si si') (aim + aim') (Data.Set.Monad.union so so')
      where
        mergeInputs (si::Set Input) si' = Data.List.map (\s -> [a | a <- s, not (isSubsetOf a (join u))]) [si, si']
          where
            --b = ( Data.Set.Monad.map (\s -> [a | a <- s, not (isSubsetOf a (join u))]) (fromList [si, si']))
            u = fromList [Data.Set.Monad.union a b | a <- si, b <- si', not $ disjoint a b]
        graphCheck aim'' = gmap (\n -> if member n diff then recLoop n else n) loops
        --overlay <function turning new diff set into graph> (map recLoop diff) (scc aim) 
          where
            loops = topSort (scc aim'')
            diff = difference (vertexSet loops) (vertexSet . scc aim)
            -- <edge relation of merged graph with which to reconstruct merged graph with updated nodes>

        recLoop subgraph  nodeSet = if isAcyclic subgraph then return nodeSet else recLoop sbgrph' (c:nodeSet)
          where

            v = case topSort subgraph of
              Right _ -> vertexList subgraph
              Left  x -> Data.List.NonEmpty.toList x
            e = edgeList subgraph
            score inc out = abs (length inc - length out)
            candidate =foldl1 (\ x@(z,_) y@(w,_) -> if z <= w then y else x)  edgecounts
            c = candidate
            mapNode y = Data.List.map (`y` subgraph)
            edgecounts = zip (zipWith score (mapNode preSet v ) (mapNode postSet v)) e
            edgeFunc sb (e1,e2) = removeEdge e1 e2 sb
            sbgrph' = Data.List.foldl edgeFunc subgraph e






        updateNodes aim'' = do
          n <- vertexSet aim
          difference (preset n aim'') (preset n aim)
          where
            list = vertexList





