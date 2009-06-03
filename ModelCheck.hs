{-# LANGUAGE MultiParamTypeClasses #-}
module ModelCheck where
import LTL
import BuchiAutomata
import KripkeAutomata

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad


class Property s p where
  eval :: p -> s -> Bool

(<$>) :: (Property s p) => Literal p -> s -> Bool
(sgn,p) <$> s = (if sgn then id else not) (eval p s)

type IntersectionState s1 s2 = (s1,s2)

getIntersectionNextStates :: (Property s p, Ord sid, Ord sb) => Kripke sid s -> Buchi sb p -> IntersectionState sid sb -> [IntersectionState sid sb]
getIntersectionNextStates kripke buchi (s,t) = [(s',t') | (t',ps) <- getBuchiNextStates buchi t, and (map (<$>getStateInfo kripke s) ps) , s' <- getKripkeNextStates kripke s]


modelcheck :: (Property s p, Ord sid, Ord sb) => Kripke sid s -> Buchi sb p -> Maybe [IntersectionState sid sb]
modelcheck kripke buchi = msum [dfs kripke buchi [] (s,initState buchi) | s <- initStates kripke]

dfs :: (Property s p, Ord sid, Ord sb)=> Kripke sid s -> Buchi sb p -> [IntersectionState sid sb] -> IntersectionState sid sb -> Maybe [IntersectionState sid sb] 
dfs kripke buchi visited st@(s,t) = let newvisited = st : visited
                                        accessible = getIntersectionNextStates kripke buchi st
                                        newaccessible = filter (not . (`elem` newvisited))  accessible
                                        recresults = map (dfs kripke buchi newvisited) newaccessible
                                    in
                                        (if t `S.member` finalStates buchi then ndfs kripke buchi newvisited [] st else mzero)
                                        `mplus`
                                        msum recresults 


ndfs :: (Property s p, Ord sid, Ord sb) => Kripke sid s -> Buchi sb p -> [IntersectionState sid sb] -> [IntersectionState sid sb] -> IntersectionState sid sb -> Maybe [IntersectionState sid sb]
ndfs kripke buchi outervisited innervisited st
         | (not.null) innervisited && st `elem` outervisited = return (reverse (st: innervisited ++ drop 1 outervisited))
         | otherwise              = let newinnervisited = st : innervisited
                                        accessible = getIntersectionNextStates kripke buchi st
                                        newaccessible = filter (not . (`elem` newinnervisited)) accessible
                                        recresults  = map (ndfs kripke buchi outervisited newinnervisited) newaccessible
                                    in  msum recresults



