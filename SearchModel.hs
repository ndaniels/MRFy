module SearchModel 
where
  
type Score = Double

infix /+/
(/+/) :: Score -> Scored a -> Scored a
x /+/ Scored a y = Scored a (x + y)

instance Eq (Scored a) where
  x == x' = scoreOf x == scoreOf x'
instance Ord (Scored a) where
  x < x' = scoreOf x < scoreOf x'

instance Functor Scored where
  fmap f (Scored a x) = Scored (f a) x

scoreOf :: Scored a -> Score
scoreOf (Scored _ x) = x
--------------------------------------------------------
data Placement = Placement String

-- @ start scoredecl.tex
data Scored a = Scored a Score
type Scorer = Placement -> Scored Placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int
data SearchStrategy = 
 SS { gen0    :: Seed -> [Placement]
    , nextGen :: Seed -> Scorer 
              -> [Scored Placement] -> [Scored Placement]
    , accept  :: Seed -> [Score] -> Age -> Bool
    , quit    ::         [Score] -> Age -> Bool
    }
-- @ end strategy.tex

--------------------------------------------------------
-- @ start search.tex
search :: SearchStrategy -> Scorer -> [Seed]
       -> (Scored Placement, [Score])
search strat score (s0:seeds) = runFrom seeds firstGen [] 0
 where
  firstGen = map score $ gen0 strat s0
  runFrom :: [Seed] -> [Scored Placement] -> [Score]
          -> Age -> (Scored Placement, [Score])
  runFrom (s1:s2:seeds) oldPop oldHist age =
    let trialPop  = nextGen strat s1 score oldPop
        trialHist = (scoreOf $ minimum newPop) : oldHist
        (newPop, newHist) =
          if accept strat s2 newHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then
          (minimum newPop, newHist)
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex

-- the following HOF can used to adapt strategy functions that use the old interface
adapt :: ([Scored Age] -> Age -> a) -> ([Score] -> Age -> a)
adapt f scores n = f (zipscore (iterate (flip (-) 1) n) scores) n
  where zipscore (n:ns) (s:ss) = Scored n s : zipscore ns ss
        zipscore _ _ = []

