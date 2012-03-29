module Score
       ( Score(..), negLogZero, negLogOne, unScore
       , Scored(..), (/+/)
       )
where
  
newtype Score = Score Double
  deriving (Eq, Ord)
 -- ^ A "score" is the negated logarithm of a probability
                
negLogZero :: Score -- ^ Stands in for - log 0
negLogZero = Score 10e1024

negLogOne :: Score -- ^ - log 1
negLogOne = Score 0.0

unScore :: Score -> Double
unScore (Score x) = x

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

-- @ start vscore.tex
data Scored a = Scored { unScored :: !a, scoreOf :: !Score}
(/+/) :: Score -> Scored a -> Scored a
-- @ end vscore.tex
infix /+/
x /+/ Scored a y = Scored a (x + y)

instance Eq (Scored a) where
  x == x' = scoreOf x == scoreOf x'
instance Ord (Scored a) where
  x < x' = scoreOf x < scoreOf x'
instance Show a => Show (Scored a) where
  show s = "(Score: " ++ (show $ scoreOf s) ++ ", " ++ (show $ unScored s) ++ ")"

instance Functor Scored where
  fmap f (Scored a x) = Scored (f a) x

