module BuchiAutomata where
import LTL -- should not?

import Data.Set
import qualified Data.Map as M


data Buchi sID prop = Buchi 
                       { initState   :: sID
                       , finalStates :: Set sID
                       , arrows      :: M.Map sID [(sID,[Literal prop])]
                       }

getBuchiNextStates :: (Ord sID) => Buchi sID p -> sID -> [(sID,[Literal p])]
getBuchiNextStates buchi t = maybe [] id (M.lookup t (arrows buchi))


