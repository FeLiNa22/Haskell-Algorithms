module LSystems where

import IC.Graphics
import Data.Fixed

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (angle, _, _) = angle

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_, axiom, _) = axiom

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_, _, rules) = rules

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list and rule list is not empty
lookupChar c rules = head $ [exp | (binding, exp)<-rules, binding == c]

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne binding rules = concat $ map replaceBinding binding
  where
    replaceBinding c    = lookupChar c rules

-- Expand command string s n times using rule table r
-- Pre : n >= 1
expand :: String -> Int -> Rules -> String
expand binding 0 _       = binding
expand binding n rules   = expand expanded_binding (n-1) rules
      where
        expanded_binding = expandOne binding rules

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move c a ((x,y),orientation)
  | c == 'F' = (((x + cos rads), (y + sin rads)), orientation)
  | c == 'L' = ((x,y), normalisedOrientation (orientation + a))
  | c == 'R' = ((x,y), normalisedOrientation (orientation - a))
  where
    rads = (orientation * pi) / 180
    normalisedOrientation theta
      | theta >= 360 = normalisedOrientation (theta - 360)
      | theta < 0    = normalisedOrientation (theta + 360)
      | otherwise    = theta
--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--

initTurtleState = ((0,0),90)

trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 commands a colour = colouredlines
    where 
      (colouredlines, _) = trace' initTurtleState commands
      trace' _ [] = ([],[])
      trace' currentTurtleState@((x1,y1),c1) (command:xs_commands)
        | command == 'F' = (((x1,y1),(x2,y2),colour):trace, next_xs_comms)
        | command == ']' = ([], xs_commands)
        | command == '[' = (branch_trace ++ xs_trace, xs_comms)
        | otherwise      = trace' nextTurtleState xs_commands
        where
          nextTurtleState@((x2,y2),c2)  = move command a currentTurtleState
          (branch_trace, branch_xs_comms) = trace' currentTurtleState xs_commands
          (xs_trace, xs_comms) = trace' currentTurtleState branch_xs_comms
          (trace, next_xs_comms) = trace' nextTurtleState xs_commands

trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands a colour = trace' initTurtleState commands [initTurtleState] 
    where
      trace' _ []  _     = []
      trace' currentTurtleState@((x1,y1),c1) (command:xs_commands) stack@(top:xs_stack) 
        | command == 'F' = ((x1,y1),(x2,y2),colour) : trace' nextTurtleState xs_commands stack 
        | command == ']' = trace' top xs_commands xs_stack 
        | command == '[' = trace' currentTurtleState xs_commands (currentTurtleState:stack) 
        | otherwise      = trace' nextTurtleState xs_commands stack 
        where
          nextTurtleState@((x2,y2),c2) = move command a currentTurtleState


colours = [black, blue, green, cyan, red, magenta, yellow, white]
-- ext work
trace3 :: Commands -> Angle -> [ColouredLine]
trace3 commands a = trace' initTurtleState commands [(initTurtleState,(0, 0, 1))] 
    where
      trace' _ [] _    = []
      trace' currentTurtleState@((x1,y1),c1) (command:xs_commands) stack@((top,colour):xs_stack) 
        | command == 'F' = ((x1,y1),(x2,y2),colour) : trace' nextTurtleState xs_commands stack 
        | command == ']' = trace' top xs_commands xs_stack
        | command == '[' = trace' currentTurtleState xs_commands ((currentTurtleState,next_colour):stack)
        | otherwise      = trace' nextTurtleState xs_commands stack 
        where
          nextTurtleState@((x2,y2),c2) = move command a currentTurtleState
          next_colour = nextColour colour
            
      nextColour (r,g,b) 
          | r == 1 = (0,1,0) 
          | g == 1 = (0,0,1) 
          | b == 1 = (1,0,0) 

----------------------------------------------------------
-- Some given functions
----------------------------------------------------------

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

drawLSystem3 :: LSystem -> Int -> IO ()
drawLSystem3 system n 
  = drawLines (trace3 (expandLSystem system n) (angle system) )

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
