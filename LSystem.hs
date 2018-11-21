{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- | L-System generative grammar
-- | Written by Sofia Wyetzner
-- |
-- | Note: Initial implementation inspiration from Lindenmayer-D0L package 
-- | (http://hackage.haskell.org/package/lindenmayer-0.1.0.1/docs/Lindenmayer-D0L.html)
--
import Data.Foldable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Turtle

main :: IO ()
main = mainWith (sketch runT :: Diagram B)

-- | Data class 'LSystem' with 'axiom' and 'rules' functions
data LSystem a = LSystem {
    axiom :: [a],
    rules :: a -> [a]
}

-- | Show instance of 'LSystem'. Shows axiom result.
instance (Show a) => Show (LSystem a) where
    show (LSystem ax _) = show ax

-- | Basic Algae growth example.
algae :: LSystem Char
algae = LSystem "A" rule
    where rule 'A' = "AB"
          rule 'B' = "A"

-- | Fractal tree example. Uses constants so there is a default rule.
fractalTree :: LSystem Char
fractalTree = LSystem "0" rule
    where rule '0' = "1[0]0"
          rule '1' = "11"
          rule  x  = [x]

dragon :: LSystem Char
dragon = LSystem "FX" rule
    where rule 'F' = "Z"
          rule 'X' = "FX+FY+"
          rule 'Y' = "-FX-FY"
          rule  x  = [x]

sierpinski :: LSystem Char
sierpinski = LSystem "A" rule
    where rule 'A' = "B-A-B"
          rule 'B' = "A+B+A"
          rule  x  = [x]

fern :: LSystem Char
fern = LSystem "X" rule
    where rule 'X' = "F+[[X]-X]-F[-FX]+X"
          rule 'F' = "FF"
          rule  x  = [x]

-- | Single step of rule application.
grow :: LSystem a -> LSystem a
grow (LSystem axiom rule) = LSystem (axiom >>= rule) rule

-- | N steps of rule application.
growN :: Int -> LSystem a -> LSystem a
growN n x = iterate grow x !! n


parseDragon :: LSystem Char -> Turtle Double ()
parseDragon = traverse_ turtle . axiom
    where turtle :: Char -> Turtle Double ()
          turtle x = case x of
            'F' -> penDown >> forward 1
            'f' -> penUp   >> forward 1
            '+' -> penDown >> right 90
            '-' -> penDown >> left 90
            _   -> return ()

parseSierpinski :: LSystem Char -> Turtle Double ()
parseSierpinski = traverse_ turtle . axiom
    where turtle :: Char -> Turtle Double ()
          turtle x = case x of
            'A' -> penDown >> forward 1
            'B' -> penDown >> forward 1
            '+' -> penDown >> left 60
            '-' -> penDown >> right 60
            _   -> return ()


parseFern :: TurtleStack Double () -> LSystem Char -> Turtle Double ()
parseFern stack = traverse_ turtle . axiom
    where turtle :: Char -> Turtle Double ()
          turtle x = case x of
            'F' -> penDown >> forward 1
            '-' -> penDown >> left 25
            '+' -> penDown >> right 25
            '[' -> push stack penDown
            ']' -> pop stack

sketch :: Turtle Double a -> QDiagram B V2 Double Any
sketch = lwL 0.2 . stroke . sketchTurtle

runT :: Turtle Double ()
runT = parseSierpinski (growN 6 sierpinski)

type TurtleStack n a = [Turtle n a]

emptyStack :: TurtleStack n a
emptyStack = []

push :: TurtleStack n a -> Turtle n a -> TurtleStack n a
push stack t = t:stack

pop :: TurtleStack n a -> (TurtleStack n a, Turtle n a)
pop stack = (tail stack, head stack)


-- TODO: The Diagrams Turtle package doesn't support stack operations so 
-- I need to write a custom Turtle renderer with Graphics



