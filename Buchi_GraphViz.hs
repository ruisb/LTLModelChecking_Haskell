{-#LANGUAGE FlexibleInstances#-}
module Buchi_GraphViz where
import BuchiAutomata

import Data.GraphViz

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)

print_buchi2dot ::  (Ord sID, Integerable sID, Show prop) => Buchi sID prop -> GraphvizOutput -> FilePath -> IO Bool
print_buchi2dot b out fl = runGraphviz (buchi2dot b) out fl

buchi2dot :: (Ord sID, Integerable sID, Show prop) => Buchi sID prop -> DotGraph
buchi2dot buchi = DotGraph { directedGraph   = True
                           , graphAttributes = []
                           , graphNodes      = map mk_node (M.keys (arrows buchi))
                           , graphEdges      = map (\(x,(y,p)) -> mk_edge x y p) . concat 
                                             . map (\(x,l) -> map ((,) x) l) . M.toList $ arrows buchi
                           }
   where mk_node s = DotNode { nodeID = showInt s
                             , nodeAttributes = [FillColor (RGB 0 255 0)|s==initState buchi] ++ [Shape Doublecircle |s `S.member` finalStates buchi]
                             }
         mk_edge x y ps = DotEdge { edgeHeadNodeID = showInt y
                                       , edgeTailNodeID = showInt x
                                       , edgeAttributes = [Label (intercalate " & " $ map (\(s,p)->showSgn s ++ show p) ps)]
                                       , directedEdge   = True
                                       }
         showSgn False = "~"
         showSgn True  = "" 



class Integerable a where
   showInt :: a -> Int

instance Integerable Int where
   showInt = id

instance Integerable (Int,Int) where
   showInt (x,y) = ((x+y)*(x+y+1) `div` 2) + y


