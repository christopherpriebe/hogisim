module Bool where

xor :: Bool -> Bool -> Bool
xor a b = a /= b

nor :: Bool -> Bool -> Bool
nor False False = True
nor _ _ = False

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

xnor :: Bool -> Bool -> Bool
xnor a b = a == b