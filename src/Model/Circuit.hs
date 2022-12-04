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
--import Data.Lens
{-data Gate 
    = Input Bool
    | Output Bool
    | Unary (Bool -> Bool)
    | Binary (Bool -> Bool -> Bool)
-}
--first number corresponds to the gate number, the second corresponds to the gate input number
data EndPt  = EP (Set Int) (SF a Bool)
data Node = N Int (Bool->Bool-> Bool) (forall a. SF a Bool)
data NodeGraph = NG {domain :: Set Node, relation ::  HashMap Int (Set Int)}

emNG :: NodeGraph
emNG = NG {domain = empty, relation = empty}
data Circuit where
  C :: Set EndPt () -> NodeGraph -> Set EndPt Bool -> Circuit


instance Graph (Circuit) where
    type Vertex (Circuit) = Node
    empty = let x = (constant false) in C (EP empty x) EmNG (EP empty x)
    vertex (num,f) = C (EP (singleton num) stream) NG{domain =singleton $ Node num f, relation = empty } (EP (singleton num))
        where
            stream = Constant false
            
    overlay (NG i sf) (Node j sf') =


{-- data Input = Input Int Bool
type Output Input

data Circuit = 
    | Inp Input
    | Out Output
    | BiGate PrimitiveBinaryGate Circuit Circuit
    | UnGate PrimitiveUnaryGate Circuit

    
data PrimitiveUnaryGate = NOT | DELAY

data PrimitiveBinaryGate = 
    | AND 
    | OR 
    | NOR
    | NAND
    | XOR 
    | XNOR

solve :: Gate -> Bool
solve (Input _ value) = value
solve (Unary _ gate operation) = operation (solve gate)
solve (Binary _ gateA gateB operation) = operation (solve gateA) (solve gateB)

xor :: Bool -> Bool -> Bool
xor True b = not b
xor a True = not a
xor _ _ = False

a = Input 0 False
b = Input 1 False
carry = Input 2 True

xor1 = Binary 3 a b xor
outSum = Binary 4 xor1 carry xor
and1 = Binary 5 carry xor1 (&&)
and2 = Binary 6 a b (&&)
outCarry = Binary 7 and1 and2 (||) --}

-- >>> solve outCarry
-- False

-- >>> solve outSum
-- True

