module KripkeAutomata where

import Data.Maybe
import qualified Data.Map as M

data Kripke sid s = Kripke { initStates    :: [sid]
                           , transitions   :: M.Map sid [sid]
                           , stateInfo     :: M.Map sid s
                           }

getKripkeNextStates :: (Ord sid) => Kripke sid s -> sid -> [sid]
getKripkeNextStates kripke s = maybe [] id  (M.lookup s (transitions kripke))

getStateInfo :: (Ord sid) => Kripke sid s -> sid -> s
getStateInfo kripke s = fromJust . M.lookup s . stateInfo $ kripke

