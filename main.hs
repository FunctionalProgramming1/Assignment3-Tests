-- DO NOT MODIFY THIS FILE

import qualified Assignment3

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install QuickCheck

import Test.QuickCheck -- see https://hackage.haskell.org/package/QuickCheck for
                       -- documentation if you want to write your own tests

import Control.Monad(liftM)
import Data.List(nub,sort)

-- Tests

-- Exercise 1

_ = Assignment3.Apple :: Double -> Assignment3.Fruit
_ = Assignment3.Banana :: Double -> Assignment3.Fruit
_ = Assignment3.Lemon :: Integer -> Assignment3.Fruit

_ = Assignment3.sumPrice :: [Assignment3.Fruit] -> Double -> Double -> Double -> Double

instance Arbitrary Assignment3.Fruit where
  arbitrary = oneof [liftM Assignment3.Apple arbitrary,
                     liftM Assignment3.Banana arbitrary,
                     liftM Assignment3.Lemon arbitrary]

sumPrice :: [Assignment3.Fruit] -> Double -> Double -> Double -> Double
sumPrice []                          _ _ _ = 0.0
sumPrice (Assignment3.Apple x  : xs) a b l = x * a + sumPrice xs a b l
sumPrice (Assignment3.Banana x : xs) a b l = x * b + sumPrice xs a b l
sumPrice (Assignment3.Lemon x  : xs) a b l = fromInteger x * l + sumPrice xs a b l

prop_Exercise1 xs a b l = abs (Assignment3.sumPrice xs a b l - sumPrice xs a b l) < 1e-10

-- Exercise 2

_ = Assignment3.Void :: Assignment3.BSTree
_ = Assignment3.BSNode :: Assignment3.BSTree -> Integer -> Assignment3.BSTree -> Assignment3.BSTree

_ = Assignment3.subTree :: Integer -> Integer -> Assignment3.BSTree -> Assignment3.BSTree

instance Arbitrary Assignment3.BSTree where
  arbitrary = do
    xs <- arbitrary
    return $ foldl insertBSTree Assignment3.Void (nub xs)

insertBSTree :: Assignment3.BSTree -> Integer -> Assignment3.BSTree
insertBSTree Assignment3.Void m = Assignment3.BSNode Assignment3.Void m Assignment3.Void
insertBSTree (Assignment3.BSNode l x r) m | m<x = Assignment3.BSNode (insertBSTree l m) x r
                                          | x<m = Assignment3.BSNode l x (insertBSTree r m)
                                          | otherwise = error "duplicate label"

toList t = toList' t []
  where toList' Assignment3.Void acc = acc
        toList' (Assignment3.BSNode l x r) acc = toList' l (x : toList' r acc)

prop_Exercise2 a b t = toList (Assignment3.subTree a b t) == [ x | x <- toList t, a <= x && x < b ]

-- Exercise 3

_ = Assignment3.Node :: a -> [Assignment3.Tree a] -> Assignment3.Tree a

_ = Assignment3.count :: Assignment3.Tree a -> Integer
_ = Assignment3.labels :: Assignment3.Tree a -> [a]
_ = Assignment3.height :: Assignment3.Tree a -> Integer

instance Arbitrary a => Arbitrary (Assignment3.Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Assignment3.Tree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (Assignment3.Node t ts)

count (Assignment3.Node _ xs) = 1 + count' xs
  where
    count' []     = 0
    count' (x:xs) = count x + count' xs

labels (Assignment3.Node x xs) = x : labels' xs
  where
    labels' []     = []
    labels' (x:xs) = Main.labels x ++ labels' xs

height (Assignment3.Node _ []) = 0
height (Assignment3.Node _ xs) = 1 + height' xs
  where
    height' []     = 0
    height' (x:xs) = max (height x) (height' xs)

prop_Exercise3_count t = Assignment3.count t == count (t :: Assignment3.Tree Int)

prop_Exercise3_labels t = sort (Assignment3.labels t) == sort (Main.labels (t :: Assignment3.Tree Int))

prop_Exercise3_height t = Assignment3.height t == height (t :: Assignment3.Tree Int)

-- Exercise 4

_ = (Assignment3.++) :: [a] -> [a] -> [a]
a3elem :: Eq a => a -> [a] -> Bool
a3elem = Assignment3.elem
_ = Assignment3.last :: [a] -> a
_ = Assignment3.reverse :: [a] -> [a]
_ = Assignment3.filter :: (a -> Bool) -> [a] -> [a]

prop_Exercise4_append xs ys = (Assignment3.++) xs ys == xs ++ (ys :: [Int])
prop_Exercise4_elem x xs = Assignment3.elem x xs == elem x (xs :: [Int])
prop_Exercise4_last xs = null xs || Assignment3.last xs == last (xs :: [Int])
prop_Exercise4_reverse xs = Assignment3.reverse xs == reverse (xs :: [Int])
prop_Exercise4_filter p xs = Assignment3.filter (applyFun p) xs == filter (applyFun p) (xs :: [Int])

-- main

main = do
  putStrLn "Exercise 1:"
  quickCheck prop_Exercise1
  putStrLn "Exercise 2:"
  quickCheck prop_Exercise2
  putStrLn "Exercise 3:"
  putStr "count: "
  quickCheck prop_Exercise3_count
  putStr "labels: "
  quickCheck prop_Exercise3_labels
  putStr "height: "
  quickCheck prop_Exercise3_height
  putStrLn "Exercise 4:"
  putStr "(++): "
  quickCheck prop_Exercise4_append
  putStr "elem: "
  quickCheck prop_Exercise4_elem
  putStr "last: "
  quickCheck prop_Exercise4_last
  putStr "reverse: "
  quickCheck prop_Exercise4_reverse
  putStr "filter: "
  quickCheck prop_Exercise4_filter
