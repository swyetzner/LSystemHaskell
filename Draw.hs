-- | L-System generative grammar
-- | Written by Sofia Wyetzner
-- |
--
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Path.Turtle.Internal
import Control.Monad.State
import LSystem


-- | Action data type: push, pop, do nothing, and execute standard Turtle move.
data Action = PUSH | POP | NIL | EXEC Move

-- | Type to hold a standard Turtle move in a DrawState change.
type Move = DrawState -> DrawState

-- | Our stack of DrawStates, to pop and push Turtle position and orientation.
type TurtleStack = [DrawState]

-- | Type to hold the state of our turtle.
type DrawState = TurtleState Double

-- | LSystemState data type: Holds our current turtle state and our turtle state stack.
data LSystemState = STATE DrawState TurtleStack


-- | MAIN
main :: IO ()
main = mainWith (getLSDiagram fern :: Diagram B)


-- | Empty LSystem state to start from
startState :: LSystemState
startState = STATE (setHeading 90 startTurtle) []

-- | Recursivley iterate over grammar production and run LSystemState
runLSystem :: Axiom -> (Char -> Action) -> State LSystemState DrawState
runLSystem [] _ = do
    (STATE dst _) <- get
    return dst
runLSystem (a:ax) rules = do 
    evaluateMove (rules a)
    runLSystem ax rules

-- | Get Turtle Diagram from grammar production
getLSDiagram :: Axiom -> QDiagram B V2 Double Any
getLSDiagram ax = lwL 0.2 . getTurtleDiagram . fst . 
        runState (runLSystem ax fernEvalRules) $ startState

-- | Push current DrawState to LSystemState stack
pushTurtleStateToStack :: State LSystemState DrawState
pushTurtleStateToStack = state $
    \(STATE dst stk)
        -> (dst, STATE dst (dst:stk))

-- | Pop the top of the stack and update current DrawState
popTurtleStateFromStack :: State LSystemState DrawState
popTurtleStateFromStack = state $
    \(STATE dst ((TurtleState _ pos head _ _ _):stk))
        -> ((penHop dst { heading = head, penPos = pos }), STATE (penHop dst { heading = head, penPos = pos }) stk)

-- | Takes an Action and updates state
evaluateMove :: Action -> State LSystemState DrawState
evaluateMove PUSH = pushTurtleStateToStack
evaluateMove POP  = popTurtleStateFromStack
evaluateMove (EXEC mov) = state $
    \(STATE dst stk)
        -> (mov dst, STATE (mov dst) stk)
evaluateMove NIL = state $
    \(STATE dst stk)
        -> (dst, STATE dst stk)

--
-- | Example rule systems
--

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

