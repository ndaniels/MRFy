{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module Dot ( DotBuilder, node, edge, toDot )
where
import Data.List
import Control.Monad.State
import Text.Printf (printf)
 
data Node = Node Int String
data Edge = Edge Node Node String
data MyState      = Graph { n :: Int, nodes :: [Node], edges :: [Edge] }
type DotBuilder a = State MyState a

node :: String -> DotBuilder Node
edge :: Node -> Node -> String -> DotBuilder ()
node lbl = do s <- get
              let node = Node (n s) lbl
              put $ s { n = n s + 1, nodes = node : nodes s }
              return node

edge n1 n2 lbl = do s <- get
                    put $ s { edges = Edge n1 n2 lbl : edges s }
            
startState :: MyState
startState = Graph 1 [] []


toDot :: DotBuilder () -> String
toDot db = intercalate "\n"
           $ [ "digraph {" ] ++
              map nodeString (reverse $ nodes graph) ++
              map edgeString (edges graph) ++
              [ "}" ]
  where ((), graph) = runState db startState
        nodeString (Node n "")  = "  N" ++ show n
        nodeString (Node n lbl) = printf "  N%d [label=\"%s\"]" n lbl
        edgeString (Edge f t "") = printf "  N%d -> N%d" (nn f) (nn t)
        edgeString (Edge f t lbl) = printf "  N%d -> N%d [label=\"%s\"]" (nn f) (nn t) lbl
        nn (Node n _) = n
