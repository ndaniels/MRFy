module SearchModel 
where
  
type Score = Double

data Scored a = Scored a Score
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
type Scorer = Placement -> Scored Placement
-- @ end scoredecl.tex
-- @ start strategy.tex
type Age  = Int -- number of generations explored
type Seed = Int
data SearchStrategy = 
 SS { gen0    :: Seed -> [Placement]
    , nextGen :: Seed -> Scorer 
              -> [Scored Placement] -> [Scored Placement]
    , accept  :: Seed -> [Scored Age] -> Age -> Bool
    , quit    ::         [Scored Age] -> Age -> Bool
    }
-- @ end strategy.tex

--------------------------------------------------------
-- @ start search.tex
search :: SearchStrategy -> Scorer -> [Seed]
       -> (Scored Placement, [Scored Age])
search strat score (s0:seeds) = runFrom seeds firstGen [] 0
 where
  firstGen = map score $ gen0 strat s0
  runFrom :: [Seed] -> [Scored Placement] -> [Scored Age]
          -> Age -> (Scored Placement, [Scored Age])
  runFrom (s1:s2:seeds) oldPop oldHist age =
    let trialPop  = nextGen strat s1 score oldPop
        trialHist = (fmap (const age) $ minimum trialPop)
                  : oldHist
        (newPop, newHist) =
          if accept strat s2 trialHist age then
            (trialPop, trialHist)
          else
            (oldPop, oldHist)
    in  if quit strat newHist age then
          (minimum newPop, newHist)
        else
          runFrom seeds newPop newHist (age + 1)
-- @ end search.tex
