module LSystem (
  LSystem,
  Axiom,
  grow,
  growN,
  fernGrowRules,
  dragonGrowRules,
  dragon,
  fern) where

-- | L-System generative grammar
-- | Written by Sofia Wyetzner
-- |
-- | Note: Initial implementation inspiration from Lindenmayer-D0L package 
-- | (http://hackage.haskell.org/package/lindenmayer-0.1.0.1/docs/Lindenmayer-D0L.html)
--

-- | Data class 'LSystem' with 'axiom' and 'rules' functions
data LSystem a = LSystem {
    axiom :: [a],
    rules :: a -> [a]
}
-- | Axiom type to hold grammar production.
type Axiom = [Char]



-- | Single step of rule application.
grow :: LSystem a -> LSystem a
grow (LSystem axiom rule) = LSystem (axiom >>= rule) rule

-- | N steps of rule application.
growN :: Int -> LSystem a -> LSystem a
growN n x = iterate grow x !! n

-- | Example grammar rules
fernGrowRules :: LSystem Char
fernGrowRules = LSystem "X" rule
    where rule 'X' = "F+[[X]-X]-F[-FX]+X"
          rule 'F' = "FF"
          rule  x  = [x]

dragonGrowRules :: LSystem Char
dragonGrowRules = LSystem "FX" rule
    where rule 'F' = "Z"
          rule 'X' = "FX+FY+"
          rule 'Y' = "-FX-FY"
          rule  x  = [x]

-- | Example grammar productions
dragon :: Axiom
dragon = axiom $ growN 10 dragonGrowRules

fern :: Axiom
fern = axiom $ growN 6 fernGrowRules
