-- | L-System generative grammar
-- | Written by Sofia Wyetzner
-- |
-- | Note: Initial implementation inspiration from Lindenmayer-D0L package 
-- | (http://hackage.haskell.org/package/lindenmayer-0.1.0.1/docs/Lindenmayer-D0L.html)
--
import Data.Foldable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Turtle.Internal
import Control.Monad.State

-- | Data class 'LSystem' with 'axiom' and 'rules' functions
data LSystem a = LSystem {
    axiom :: [a],
    rules :: a -> [a]
}

type Axiom = [Char]

data Action = PUSH | POP | EXEC Move | NIL
type Move = DrawState -> DrawState

type TurtleStack = [DrawState]

type DrawState = TurtleState Double
data LSystemState = STATE DrawState TurtleStack

main :: IO ()
main = mainWith (getLSDiagram fern :: Diagram B)

startState :: String -> LSystemState
startState ax = STATE (setHeading 90 startTurtle) []

runLSystem :: Axiom -> (Char -> Action) -> State LSystemState DrawState
runLSystem [] _ = do
    (STATE dst _) <- get
    return dst
runLSystem (a:ax) rules = do 
    evaluateMove (rules a)
    runLSystem ax rules


getLSDiagram :: [Char] -> QDiagram B V2 Double Any
getLSDiagram x = lwL 0.2 . getTurtleDiagram . fst . 
        runState (runLSystem x fernEvalRules) $ startState x

pushTurtleStateToStack :: State LSystemState DrawState
pushTurtleStateToStack = state $
    \(STATE dst stk)
        -> (dst, STATE dst (dst:stk))


popTurtleStateFromStack :: State LSystemState DrawState
popTurtleStateFromStack = state $
    \(STATE dst ((TurtleState _ pos head _ _ _):stk))
        -> ((penHop dst { heading = head, penPos = pos }), STATE (penHop dst { heading = head, penPos = pos }) stk)


evaluateMove :: Action -> State LSystemState DrawState
evaluateMove PUSH = pushTurtleStateToStack
evaluateMove POP  = popTurtleStateFromStack
evaluateMove (EXEC mov) = state $
    \(STATE dst stk)
        -> (mov dst, STATE (mov dst) stk)
evaluateMove NIL = state $
    \(STATE dst stk)
        -> (dst, STATE dst stk)

fernEvalRules :: Char -> Action
fernEvalRules x = case x of
    'X' -> NIL
    'F' -> EXEC (forward 1)
    '-' -> EXEC (left 25)
    '+' -> EXEC (right 25)
    '[' -> PUSH
    ']' -> POP
    _   -> NIL

dragonEvalRules :: Char -> Action
dragonEvalRules x = case x of
    'F' -> EXEC (penDown >> forward 10)
    'f' -> EXEC (penUp   >> forward 10)
    '+' -> EXEC (penDown >> right 90)
    '-' -> EXEC (penDown >> left 90)
    _   -> NIL

-- | Single step of rule application.
grow :: LSystem a -> LSystem a
grow (LSystem axiom rule) = LSystem (axiom >>= rule) rule

-- | N steps of rule application.
growN :: Int -> LSystem a -> LSystem a
growN n x = iterate grow x !! n

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

dragon :: [Char]
dragon = axiom $ growN 10 dragonGrowRules

fern :: [Char]
fern = axiom $ growN 6 fernGrowRules


