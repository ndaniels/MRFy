
import Control.Monad.LazyRandom
import Control.Parallel.Strategies
import ParRandom

import Data.Time.Clock
import System.IO
import Data.Array
import Data.List as DL
import qualified Data.Vector.Unboxed as U
import System.Console.CmdArgs
import System.Random (getStdGen, mkStdGen, randoms)
import qualified Data.Vector.Unboxed as V hiding (map)
import System.Environment
-- import Test.QuickCheck

import Control.Parallel
import Control.DeepSeq

import Data.MemoTrie
-- import Data.MemoCombinators as Memo
import qualified Data.Map as M
import System.IO.Unsafe
import Data.Maybe

import Data.Number.Erf

import Bio.Sequence
import Bio.Sequence.Fasta

import Beta
import FileOps (outputAlignment)
import CommandArgs
import Constants
import HMMArby
import HMMPlus
import HMMProps
import HyperTriangles
import LazySearchModel
import MRFTypes
import Perturb
import RunPsiPred
import PsiPred
import Score
import SearchStrategy (tickProp)
import ShowAlignment
import StochasticSearch
import Viterbi
import V2 (Tree(..), costTree)

import System.Random

-- CODE DRAWN FROM ORIGINAL MRFy
loadTestData :: Files -> IO (HMMHeader, HMM, [QuerySequence])
loadTestData files =
  do querySeqs <- readFasta $ fastaF files
     mrf <- parseMRF $ hmmPlusF files
     return (hmmHeader $ checkMRF mrf, hmm $ checkMRF mrf, map (translateQuery . toStr . seqdata) querySeqs)

translateQuery :: String -> QuerySequence
translateQuery = V.fromList . map lookup
  where lookup k = case V.elemIndex k Constants.amino of
                        Just i -> AA i
                        Nothing -> error "Residue not found in alphabet"

-- CODE DRAWN FROM LOCAL SEARCH LIBRARY
type Stream a = [a]

-- temp is a new function (hence the name temp) it does repeated application of a strategy for a finite number of step
finiteSteps n f startPoints = let as = inter startPoints (map (take (n-1)) $ chunk n (f as))                    
                       in as
  where
    inter xs ys = (head xs):(head ys) ++ inter (tail xs) (tail ys)   

everyNth :: Int->Stream a->Stream a
everyNth n = map last . chunk n     
convergenceCheck :: Eq a=>Int->Stream a->Stream a
convergenceCheck width as = map fst . (\(m,n)->m++[head n]) . break (\(x,y)->x==y) $ zip as (drop width as) 
keepBest :: Ord a=>Stream a->Stream a
keepBest (x:xs) = scanl min x xs
loopP :: (Stream a->Stream a)->a->Stream a
loopP f x = let as = x:f as in as
loopS f xs = let as = xs++f as in as 
stretch n = concatMap (replicate n)
chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)
doMany :: Int->(Stream a->Stream b)->Stream a->Stream [b]
doMany n f = chunk n . f . stretch n
improvement nF xs = zipWith (\ns x->filter (<x) ns) (nF xs) xs

parDoMany :: NFData b=>Int->(Stream a->Stream b)->Stream a->Stream [b]
parDoMany n f = map parChunks . doMany n f
  where parChunks x = x `using` parList rdeepseq

divide bs xs = [[ x | (b,x)<-zip bs xs,b==i] | i <-[False,True]]
join bs xss = unfoldr f (bs,xss)
  where
    f (False:ts,[x:xs,ys]) = Just (x,(ts,[xs,ys]))
    f (True:ts,[xs,y:ys]) = Just (y,(ts,[xs,ys]))
nest bs tr = join bs . zipWith ($) [id,tr] . divide bs
window sz xs = concat [zipWith take [0..sz] (repeat xs) ,map (take sz) . tails .drop (sz-1) $ xs]

-- NEW FORM OF SOLUTION CREATOR, BASED UPON NORMAL DISTRIBUTIONS, HARD CODED AT THE MOMENT
-- YIELDS A GENERATOR, OR STREAM OF PLACEMENTS TO BE TURNED INTO SCORED SOLUTIONS
basicGuesser :: RandomGen r=>r->QuerySequence->[BetaStrand]->[Placement]
basicGuesser r qs betas = filter (isValid 0 betas) randOpts
  where
    randOpts = map sort $ chunk (length betas) $ randomRs (0,U.length qs) r
    isValid lastGuess [] _ = True
    isValid lastGuess (b:bs) (o:os) | o<lastGuess = False
                                    | o>=betaSum   = False
                                    | otherwise   = isValid (o+len b) bs os
      where
        betaSum = U.length qs - sum (map len (b:bs))
    
-- NEW MUTATOR
detMutate :: QuerySequence->[BetaStrand]->Int->Int->PricedSol [Int]->PricedSol [Int]
detMutate qs bs p v s = let (as,c:cs) = splitAt p $ solution s
                            as' = concat [as,[c+v],cs]
                        in if checkPlacement qs bs as' then s{solution=as',underlyingScore = pricer s as'}
                                                       else s

checkPlacement :: QuerySequence->[BetaStrand]->Placement->Bool
checkPlacement qs = isValid 0  
  where isValid lastGuess [] _ = True
        isValid lastGuess (b:bs) (o:os) | o<lastGuess = False
                                        | o>=betaSum   = False
                                        | otherwise   = isValid (o+len b) bs os
          where
            betaSum = U.length qs - sum (map len (b:bs))

-- lifted detMutate to operate over streams, with specific pattern that I want it to have.
mutate :: QuerySequence->[BetaStrand]->Stream (PricedSol [Int])->Stream (PricedSol [Int])
mutate q b = zipWith3 (detMutate q b) (cycle (stretch 4 [0 .. length b-1])) (cycle [1,-1,2,-2]) -- ,3,-3,4,-4])

-- for a given range, and a given way to mutate values, 
-- generate every way in which the block can be moved
-- presumes that the mutate values function will give back seed, if result invalid
blockOptions :: Eq a=>(Int->Int->a->a)->(Int,Int)->a->[a]
blockOptions f (lower,upper) sol 
  = let f' v = foldl (\s p->f p v s) sol [lower..upper]
        f'' v = foldl (\s p->f p v s) sol (reverse [lower..upper])
        g x = map fst . takeWhile (\(a,b)->a/=b) $ zip x (tail x) 
        opts = (g $ map f'' [1..]) ++ (g $ map f' (map (0-) [1..])) 
    in if null opts then [sol] else opts

-- creates a stream of blocks (ranges) and then takes a stream of solutions.
-- for a solution, and a block it generates every possible way the block can be moved left or right
mutateBlock :: RandomGen g=>QuerySequence->[BetaStrand]->g->Stream (PricedSol [Int])->Stream [PricedSol [Int]]
mutateBlock qs betas g = zipWith (blockOptions (detMutate qs betas)) pairs 
  where pairs = map (\[a,b]->(a,b)) . map sort .  chunk 2 . randomRs (0,length betas-1) $ g
            
       
-- and mutate can be restricted to neighbourhood :)
neighbourhood q b = parDoMany (4*length b) (mutate q b)
                        
-- MY VERSION OF PRICED SOLUTIONS, ORDERING, EQUALITY AND PRICING
data PricedSol a = PricedSol {solution :: a,
                              underlyingScore :: Double,
                              augmentedScore :: Double,
                              pricer :: a->Double} 

instance Eq a=>Eq (PricedSol a) where
  (==) a b = solution a == solution b 

instance Eq a=>Ord (PricedSol a) where
  compare a b = compare (underlyingScore a + augmentedScore a) (underlyingScore b+augmentedScore b)

instance NFData (PricedSol a) where
  rnf p = underlyingScore p `seq` ()

mkPricing :: (a->Double)->a->PricedSol a
mkPricing f x = PricedSol x (f x) 0 f

instance Show a=>Show (PricedSol a) where
  show (PricedSol a b c _) = "PricedSol "++show a++" "++show b++" "++show c



-- choices
stochasticChoice g (x:xs) = let (i,g') = randomR (0,length x-1) g
                            in x !! i : stochasticChoice g' xs

backTrack n = concatMap (\a -> take (n-1) a++[minimum a]) . chunk n  

main :: IO()
main = do -- standard loading
          [hmmName,qName]<- getArgs >>= return . take 2      
          startTime <- getCurrentTime >>= return . diffTimeToSeconds . utctDayTime
          (header, hmm, [query]) <- loadTestData (getFiles ["testing/"++hmmName++".hmm+","testing/"++qName++".fasta"]) 
          -- create SD
          let setSD = fromIntegral (U.length query) / 8
          -- print (U.length query,setSD,length $ betas header)
          -- setup the scorer, to be stored in the solutions
          let scorer = memo (unScore . scoreOf . score hmm query (betas header))
    
          -- generate a stream of initial solutions, ready priced up 
          initialSols<-newStdGen >>= return . map (mkPricing scorer) . (\g->basicGuesser g query (betas header))
    
          
          gforchoice<-newStdGen
          gformutate<-newStdGen
          
          -- self contained local search process
          -- this performs a single deterministic iterative improvement process on each solution it is fed
          let localsearch = map ( head . head .
                                  dropWhile (\[a,b]->a>b) . 
                                  map (take 2) . tails . 
                                  loopP (\xs->zipWith safeMin xs . 
                                              improvement (neighbourhood query (betas header)) $ xs)
                                ) 

          -- The stream of outputs from the process, vs. 
          let vs = loopP ( backTrack 15 . localsearch 
                         . map minimum . parDoMany 10 ( 
                              stochasticChoice gforchoice . -- use of stochastic choice over mutate block,
                                                            -- guarantes uniform liklihood of new solution
                                                            -- from range of options
                              mutateBlock query (betas header) gformutate )
                         ) (head initialSols) 
          
          vs'<-takeFor (startTime+(30)) vs

          print (hmmName++" vs "++qName,minimum vs',length vs')   
          endTime <- getCurrentTime >>= return . diffTimeToSeconds . utctDayTime
          let winner' = minimum vs'
          let sol' = solution winner'
          let score' = augmentedScore winner'
          let winner = Scored sol' (Score score')
          let alignment = outputAlignment hmm (betas header) winner query
          putStrLn ("Score: " ++ show score')
          putStrLn alignment
  where                                       
    setPS = 20
    diffTimeToSeconds :: DiffTime -> Integer
    diffTimeToSeconds = floor . toRational

    prune w = filter (not . flip elem w)  
    safeMin x xs = if null xs then x else minimum xs

takeFor :: (NFData a, Integral target) => target -> [a] -> IO [a]
takeFor targetTime (v:vs) 
  = do endTime<-getCurrentTime  >>=  return .  floor 
                                  .  toRational . utctDayTime
       if endTime >=targetTime 
          then return []
          else (return . (v:))  =<<    ((v `using` rdeepseq) 
                                `seq`  takeFor targetTime vs)  


  

