module LTL where


type Literal prop = (Bool , prop)

--always in negated normal form. negation onl appears near literals (represented by False)
data LTL atprop =  Const Bool
                |  Lit Bool (atprop)
                |  LTL atprop :/\: LTL atprop
                |  LTL atprop :\/: LTL atprop
                |  X (LTL atprop)
                |  U (LTL atprop) (LTL atprop)
                |  R (LTL atprop) (LTL atprop)
    deriving (Eq, Ord, Read)

atomic p = Lit True p

-- to allow fot not to appear everywhere
-- automatically converts to a nnf 
 
n :: LTL prop -> LTL prop 
n (Const b)   =  Const (not b)
n (Lit sgn p) =  Lit (not sgn) p
n (p :/\: q)  =  n p :\/: n q
n (p :\/: q)  =  n p :/\: n q
n (X p)       =  X (n p)
n (p `U` q)   =  n p `R` n q
n (p `R` q)   =  n p `U` n q

-- derived operators
true     = Const True
false    = Const False 
p ==> q  = n p :\/: q
p <==> q = p ==> q :/\: q ==> p
(<>) p   = true `U` p
(@@) p   = n ((<>) (n p))
p `w` q = (@@) p :\/: (p `U` q)


isLiteral (Lit _ _)  = True
isLiteral _          = False
toLiteral (Lit s p)  = Just (s,p)
toLiteral _          = Nothing

-- display (can be improved using showsPrec
instance Show p => Show (LTL p) where
   show (Const b)     = show b
   show (Lit sign p)  = (if sign then "" else "~") ++ show p
   show (p :/\: q)    = parens $ show p ++ " /\\ " ++ show q
   show (p :\/: q)    = parens $ show p ++ " \\/ " ++ show q
   show (X p)         = "X " ++ show p
   show (p `U` q)     = parens $ show p ++ " U " ++ show q
   show (p `R` q)     = parens $ show p ++ " R " ++ show q

parens t = "("++ t ++ ")"

subformulas,children :: LTL p -> [LTL p]
subformulas x = x : concatMap subformulas (children x)
children (Const _)  = []
children (Lit _ _)  = []
children (p :/\: q) = [p,q] 
children (p :\/: q) = [p,q] 
children (X p)      = [p] 
children (p `U` q)  = [p,q] 
children (p `R` q)  = [p,q] 
