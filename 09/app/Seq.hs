-- Sequences implemented using Banker's Deques
-- following "Purely functional data structures" by C. Okasaki
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Seq (Seq((:<), (:>), Empty),
             empty, cons, snoc,
             uncons, unsnoc,
             isEmpty, length,
             fromList, toList,
             ) where

import Prelude hiding (length)
import qualified Prelude (length)

data Seq a = MkSeq !Int [a] !Int [a] 

-- export patterns
pattern Empty :: Seq a
pattern Empty <- (isEmpty -> True) where
  Empty = Seq.empty

pattern (:<) :: a -> Seq a -> Seq a
pattern x :< xs <- (Seq.uncons -> Just (x,xs)) where
  x :< xs = Seq.cons x xs

pattern (:>) :: Seq a -> a -> Seq a
pattern xs :> x <- (Seq.unsnoc -> Just (xs,x)) where
  xs :> x = Seq.snoc xs x


c :: Int
c = 3

check :: Int -> [a] -> Int -> [a] -> Seq a
check lenf f lenr r
  | lenf>c*lenr + 1
  = let i = (lenf+lenr)`div`2
        j = lenf+lenr-i
        f' = take i f
        r' = r ++ reverse (drop i f)
    in MkSeq i f' j r'
  | lenr>c*lenf+1
  = let j = (lenf+lenr)`div`2
        i = lenf+lenr-j
        r' = take j r
        f' = f ++ reverse (drop j r)
    in MkSeq i f' j r'
  | otherwise = MkSeq lenf f lenr r

empty :: Seq a
empty = MkSeq 0 [] 0 []

cons :: a -> Seq a -> Seq a
cons x (MkSeq lenf f lenr r) = check (lenf+1) (x:f) lenr r

snoc :: Seq a -> a-> Seq a
snoc  (MkSeq lenf f lenr r) x = check lenf f (lenr+1) (x:r)

uncons :: Seq a -> Maybe (a, Seq a)
uncons (MkSeq _    []     _    [])  = Nothing
uncons (MkSeq _    []     _    (x:_)) = Just (x, empty)
uncons (MkSeq lenf (x:f') lenr r)   = Just (x, check (lenf-1) f' lenr r)

unsnoc :: Seq a -> Maybe (Seq a, a)
unsnoc (MkSeq _ [] _    [])   = Nothing
unsnoc (MkSeq _ (x:_) _ [])   = Just (empty, x)
unsnoc (MkSeq lenf f lenr (x:r')) = Just (check lenf f (lenr-1) r', x)

isEmpty :: Seq a -> Bool
isEmpty (MkSeq lenf _ lenr _) = lenf+lenr == 0

fromList :: [a] -> Seq a
fromList xs = check (Prelude.length xs) xs 0 []


toList :: Seq a -> [a]
toList (MkSeq _ xs' _ xs'') = xs' ++ reverse xs''

length :: Seq a -> Int
length (MkSeq lenf _ lenr _) = lenf+lenr

