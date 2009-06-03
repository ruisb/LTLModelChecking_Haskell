{-# LANGUAGE PatternGuards #-}
module LTLtoBuchi where
import LTL
import BuchiAutomata

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Control.Arrow
import Control.Monad
import Control.Monad.State

-- monad for fresh integer identifiers
type FreshID a = State (Int,M.Map Int a)
freshID :: a -> FreshID a Int
freshID x = do {id <- gets fst ; modify ((+1) *** M.insert id x); return id}

runFreshID :: FreshID a r -> (r,M.Map Int a) 
runFreshID = runFreshIDfrom 1
runFreshIDfrom n = (id *** snd) . flip runState (n,M.empty)

--first step: give names to literals in LTL so that they are comparable
--(just necessary if they are not)
--
quote :: LTL p -> (LTL Int, M.Map Int p)
quote = runFreshID . quote'
  where quote' (Const b)   = return (Const b)
        quote' (Lit sgn p) = liftM (Lit sgn) (freshID p)
        quote' (p :\/: q)  = liftM2 (:\/:) (quote' p) (quote' q)
        quote' (p :/\: q)  = liftM2 (:/\:) (quote' p) (quote' q)
        quote' (X p)       = liftM  X      (quote' p)
        quote' (p `U` q)   = liftM2 U      (quote' p) (quote' q)
        quote' (p `R` q)   = liftM2 R      (quote' p) (quote' q)


substbuchi :: Buchi s Int -> M.Map Int p -> Buchi s p
substbuchi b m = b { arrows = M.map (map (id *** map psubst)) (arrows b)} 
   where psubst (sgn, id) = (sgn,fromJust (M.lookup id m))

ltl2buchiNE :: LTL p -> Buchi (Int,Int) p
ltl2buchiNE = uncurry substbuchi . (ltl2buchi *** id) . quote

ltl2buchi :: (Ord p) => LTL p -> Buchi (Int,Int) p
ltl2buchi = gbuchi2buchi . ltl2gbuchi

--main step : LTL to generalized buchi automaton


data GBuchi st prop = GBuchi { ginitState   :: st
                             , gfinalStates :: [S.Set st]
                             , garrows      :: M.Map st [(st,[Literal prop])]
                             } 


data IntermediateState p = IS { incoming :: S.Set Int
                              , old      :: S.Set (LTL p)
                              , new      :: [LTL p]
                              , next     :: S.Set (LTL p)
                              }

data StateInfo p = SI { inhere :: S.Set (LTL p) , onnext :: S.Set (LTL p)}
    deriving Eq




ltl2gbuchi f = let s   = IS { incoming = S.singleton 0
                            , old      = S.empty
                            , new      = [f]
                            , next     = S.empty
                            }
                   gb0 = GBuchi { ginitState   = 0
                                , gfinalStates = undefined
                                , garrows      = M.empty
                                }
                   (gb1,nl1) = runFreshIDfrom 1 (expand s gb0) 
                   chifinals = [\si->k`S.member`si || not (h `U` k `S.member` si)
                               | h `U` k <- subformulas f] 
                   final chi = M.keysSet ( M.filter (chi.inhere) nl1 )
                   allstates = M.keysSet nl1
              in  gb1 { gfinalStates = (null ? (const [allstates],id)) (map final chifinals)}
    where (?) :: (a -> Bool) -> (a -> b, a -> b) -> (a->b)
          cond ? (f,g) = \x-> if cond x then f x else g x



expand :: (Ord p) => IntermediateState p -> GBuchi Int p -> FreshID (StateInfo p) (GBuchi Int p)
expand s gb  | []     <- new s 
          = do nlst <- gets snd
               let sinfo = save s
               case find ((==sinfo).snd) (M.toList nlst) of
                 Just (rID,rnm) -> return (addArrows (incoming s) rID sinfo gb)
                 Nothing -> do sID <- freshID sinfo
                               let s' = IS { incoming = S.singleton sID
                                           , old      = S.empty
                                           , new      = S.toList (next s)
                                           , next     = S.empty
                                           } 
                               expand s' (addArrows (incoming s) sID sinfo gb) 
               | (f:fs) <- new s
               = if f `S.member` old s 
                  then expand s{new=fs} gb
                  else process f s gb 

process :: (Ord p) => LTL p -> IntermediateState p -> GBuchi Int p -> FreshID (StateInfo p) (GBuchi Int p)
process f@(Const False) s acc = return acc
process f@(Const True)  s acc = expand (consume s) acc
process f@(Lit sgn p)   s acc 
        | n f `S.member` old s = return acc             
        | otherwise            = expand (consume s) acc
process f@(p :/\: q)    s acc = let s' = (queueNews [p,q] . consume) s
                                in  expand s' acc
process f@(p :\/: q)    s acc = let s1 = (queueNew p . consume) s
                                    s2 = (queueNew q . consume) s
                                in (expand s2 <=< expand s1) acc
process f@(X p)         s acc = let s' = (queueNext p . consume) s
                                in  expand s acc
--using the equivalence p U q == q \/ (p /\ X (p U q))
process f@(p `U` q)     s acc = let s1 = (queueNext f . queueNew p . consume) s
                                    s2 = (queueNew q . consume) s
                                in  (expand s2 <=< expand s1) acc
--using the equivalence p U q == q /\ (p \/ X (p R q)) == (q /\ p) \/ (q /\ (p R q))
process f@(p `R` q)     s acc = let s1 = (queueNews [q,p] . consume) s
                                    s2 = (queueNext f . queueNew q . consume) s
                                in  (expand s2 <=< expand s1) acc


consume :: (Ord p) => IntermediateState p -> IntermediateState p
consume s = let (h:t) = new s
            in  s {old = S.insert h (old s), new = t}

queueNew :: LTL p -> IntermediateState p -> IntermediateState p
queueNew  f  s = s {new = f : new s}
queueNews :: [LTL p] -> IntermediateState p -> IntermediateState p
queueNews fs s = foldr queueNew s fs

queueNext :: (Ord p) => LTL p -> IntermediateState p -> IntermediateState p
queueNext  f  s = s {next = S.insert f (next s)}
queueNexts :: (Ord p) => [LTL p] -> IntermediateState p -> IntermediateState p
queueNexts fs s = foldr queueNext s fs

save :: IntermediateState p -> StateInfo p
save s = SI { inhere = old s, onnext = next s}

addArrows :: (Ord s) => S.Set s -> s -> StateInfo p -> GBuchi s p -> GBuchi s p
addArrows srcs tar tarinfo gb = let lab = [(sgn,p) | Lit sgn p <-  S.toList (inhere tarinfo)]
                                in  gb { garrows = foldl 
                                                    (\a s -> addArrow s tar lab a) (garrows gb) 
                                                    (S.toList srcs)} 
    where addArrow :: (Ord s) => s -> s -> [p] -> M.Map s [(s,[p])] -> M.Map s [(s,[p])]
          addArrow s t l arrs = M.alter (Just . maybe [(t,l)] ((t,l):)) s arrs



-- Generalized Buchi to Buchi Automata

gbuchi2buchi :: (Ord st) => GBuchi st p -> Buchi (st,Int) p
gbuchi2buchi gb = Buchi 
                   { initState   = (ginitState gb,0)
                   , finalStates = S.mapMonotonic (<*> 0) (head (gfinalStates gb))
                   , arrows      = let f = gfinalStates gb  
                                       k = length f
                                       states = [(s,i) | s <- M.keys (garrows gb), i<- [0..k-1]]
                                       nextTo (s,i) = [((s',inc s i),a) | (s',a) <- fromJust $ M.lookup s (garrows gb)]
                                       inc s i | s `S.member` (f!!i) = (i + 1) `mod` k 
                                               | otherwise           = i
                                   in M.fromList $ map (id &&& nextTo) states
                   }

(<*>) = (,)


