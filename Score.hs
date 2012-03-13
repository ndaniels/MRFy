module Score
       ( Score(..), negLogZero
       , Scored(..), scoreOf
       )
where
  
newtype Score = Score Double
  deriving (Eq, Ord)
 -- ^ A "score" is the negated logarithm of a probability
                
negLogZero :: Score -- ^ Stands in for - log 0
negLogZero = Score 10e1024 


instance Show Score where
  show (Score x) = show x

instance Num Score where
  Score x + Score y = Score $ x + y
  Score x - Score y = Score $ x - y
  Score _ * Score _ = error "multiplying log probabilities makes no sense"
  negate (Score _) = error "negating a Score is not permitted"
  abs (Score _) = error "absolute value of Score is senseless"
  signum (Score _) = error "signum of Score is senseless"
  fromInteger = Score . fromInteger

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
