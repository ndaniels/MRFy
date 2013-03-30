{-# LANGUAGE ScopedTypeVariables #-}
module PrefixMemo
{-
       ( isoLawOfIndex
       , listPrefixIso
       , quickCheck
       , goodLengthLaw
       )
-}
where
import Data.Array.IArray
import Data.List
import Data.MemoCombinators
import Test.QuickCheck

newtype N = N Int
  deriving (Eq, Ord, Ix, Show)

prefixLengths :: [N]
prefixLengths = map N [1..]

data Numbered a = Numbered { the_number :: N, unNumbered :: a }
                deriving (Show, Eq)

data Iso a b = Iso (a -> b) (b -> a)

insist :: Bool -> a -> a
insist True a = a
insist False _ = error "assertion failed"

numberByPrefix :: [a] -> [Numbered a]
numberByPrefix = reverse . zipWith Numbered prefixLengths . reverse

goodLengthLaw :: [a] -> Bool
goodLengthLaw as = goodLengths prefixes
  where numbered = numberByPrefix as
        prefixes = reverse $ tails numbered
        i_of_prefix [] = N 0
        i_of_prefix (Numbered n a : _) = n
        goodLengths = all (\as -> i_of_prefix as == N (length as))

listPrefixIso :: forall a . [a] -> ([Numbered a], Iso [Numbered a] N)
                           -- prefix of length N is isomorphic to that number
listPrefixIso as = insist (goodLengths prefixes) $
                   ( zipWith Numbered (map N [length as..1]) as
                   , Iso i_of_prefix prefix_of_i
                   )
  where numbered = numberByPrefix as
        prefixes = reverse $ tails numbered
        memotable :: Array N [Numbered a]
        memotable = listArray (N 0, N (length as)) prefixes
        prefix_of_i i = memotable ! i
        i_of_prefix [] = N 0
        i_of_prefix (Numbered n a : _) = n
        goodLengths = all (\as -> i_of_prefix as == N (length as))


isoLawOfIndex :: Eq a => [a] -> Bool
isoLawOfIndex as = all good [0..length as]
  where good i = (i_of_list . list_of_i) (N i) == (N i)
        (_, Iso i_of_list list_of_i) = listPrefixIso as
        
isoLawOfPrefix :: Eq a => [a] -> Bool
isoLawOfPrefix as = all good [0..length as]
  where good i = (list_of_i . i_of_list) (drop i numbered) == drop i numbered
        (_, Iso i_of_list list_of_i) = listPrefixIso as
        numbered = numberByPrefix as
        
prefixMemo :: [Numbered a] -> ( [Numbered a]
                              , ([Numbered a] -> b) -> ([Numbered a] -> b)
                              )
prefixMemo nas = (nas', wrap list_of_i i_of_list (arrayRange (N 0, N (length nas))))
  where (nas', Iso i_of_list list_of_i) = listPrefixIso $ map unNumbered nas
