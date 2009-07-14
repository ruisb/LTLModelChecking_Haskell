{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Test where
import LTL
import BuchiAutomata
import KripkeAutomata
import ModelCheck
import Main

import LTLtoBuchi
import Data.GraphViz
import Buchi_GraphViz

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow
import Control.Monad

--the tests in this file correspond to those in the exercises
-- - kripke is the kripke structure we are dealing with
-- - buchi1 and buchi2 are the buchi automata of the two formulae
--   we can use the function "modelcheck kripke buchij" to test them directly
-- - we can now also use the automatic LTL2buchi conversion
--   the results for formula1 and formula2 defined below (the ones from the exercise set) are:
-- *Test> checkPropertyNE kripke formula1
-- Nothing
-- *Test> checkPropertyNE kripke formula2
-- Just ["s0","s1","s0","s1"]
-- nothing indicates that the formula was verified while in the second case a counter-example was found


newtype State = State {content :: M.Map String Int} deriving (Show, Eq, Ord)

type Proposition a = a -> Bool

instance Property s (Proposition s) where
  eval = ($)


s0,s1,s2 :: State
s0 = State $ M.fromList [("x",1),("y",1),("waiting",0),("stable",1)]
s1 = State $ M.fromList [("x",1),("y",2),("waiting",0),("stable",0)]
s2 = State $ M.fromList [("x",2),("y",2),("waiting",1),("stable",0)]


kripke :: Kripke String State
kripke = Kripke {initStates = ["s0"], transitions = rho, stateInfo = infomap}
   where rho = M.fromList [ ("s0", ["s1"]    )
                          , ("s1", ["s0","s2"] )
                          , ("s2", ["s0","s2"] )
                          ] 
         infomap = M.fromList [("s0",s0),("s1",s1),("s2",s2)]



propStable, propXeqY, propWaiting :: Proposition State 
propStable    = maybe False id . liftM (==1) . M.lookup "stable"               . content
propWaiting   = maybe False id . liftM (==1) . M.lookup "waiting"              . content
propXeqY      = maybe False id . meq         . (M.lookup "x" &&& M.lookup "y") . content
       where meq = uncurry (liftM2 (==))

formula1 = (@@) (atomic propWaiting ==> (atomic propWaiting `w` atomic propStable))
formula2 = (<>) ((@@) (atomic propXeqY))
formula1' = (@@) (atomic "propWaiting" ==> (atomic "propWaiting" `w` atomic "propStable"))
formula2' = (<>) ((@@) (atomic "propXeqY"))



buchi1 :: Buchi Int (Proposition State)
buchi1 = Buchi {initState = 0, finalStates = S.singleton 2, arrows = arr}
  where arr = M.fromList [ (0, [ (0, [(True,const True)]  ) 
                               , (1, [(True,propWaiting)] )
                               ]                      )
                         , (1, [ (2, [(False,propWaiting), (False,propStable)])
                               ]                      )
                         , (2, [ (2, [(True,const True)]  ) 
                               ]                      )
                         ]

buchi2 :: Buchi Int (Proposition State)
buchi2 = Buchi {initState = 0, finalStates = S.singleton 1, arrows = arr}
   where arr = M.fromList [ (0, [ (0, [(True,const True)]) 
                                , (1, [(False,propXeqY)])
                                ]                     )
                          , (1, [ (0, [(True,const True)])
                                ]                     )
                          ]

-- pictures for the formulaes 1 and 2
show1 = print_buchi2dot (ltl2buchi formula1') Jpg "buchi1.jpg"
show2 = print_buchi2dot (ltl2buchi formula2') Jpg "buchi2.jpg"

formula3' =  (atomic "p" `U` (atomic "q" `U` atomic "r") )
show3 =  print_buchi2dot (ltl2buchi formula3') Jpg "buchi3.jpg"


