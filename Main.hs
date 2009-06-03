module Main where
import LTL
import BuchiAutomata
import KripkeAutomata
import LTLtoBuchi
import ModelCheck

import Control.Monad

checkProperty :: (Ord p, Ord sid, Property s p) => Kripke sid s -> LTL p -> Maybe [sid]
checkProperty k =  liftM (map fst) . modelcheck k . ltl2buchi . n

checkPropertyNE :: (Ord sid, Property s p) => Kripke sid s -> LTL p -> Maybe [sid]
checkPropertyNE k =  liftM (map fst) . modelcheck k . ltl2buchiNE . n

