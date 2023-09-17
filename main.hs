-- DO NOT MODIFY THIS FILE

import qualified Assignment3

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install QuickCheck

import Test.QuickCheck -- see https://hackage.haskell.org/package/QuickCheck for
                       -- documentation if you want to write your own tests

import Control.Monad(liftM2,liftM3)
import Data.List(nub,sort)

-- Tests

-- Exercise 1

_ = Assignment3.Bicycle :: String -> Integer -> Assignment3.Vehicle
_ = Assignment3.Car :: String -> Integer -> Double -> Assignment3.Vehicle
_ = Assignment3.Airplane :: String -> Integer -> Double -> Assignment3.Vehicle

_ = Assignment3.fastestBelow :: Integer -> [Assignment3.Vehicle] -> Maybe Assignment3.Vehicle

genNonnegative :: (Arbitrary a, Num a, Ord a) => Gen a
genNonnegative = fmap abs arbitrary `suchThat` (>= 0)

instance Arbitrary Assignment3.Vehicle where
  arbitrary = oneof [liftM2 Assignment3.Bicycle arbitrary genNonnegative,
                     liftM3 Assignment3.Car arbitrary genNonnegative genNonnegative,
                     liftM3 Assignment3.Airplane arbitrary genNonnegative genNonnegative]

prop_Exercise1 (NonNegative t) vs = case Assignment3.fastestBelow t vs of
    Nothing -> null (filter (\v -> price v <= t) vs)
    Just v' -> v' `elem` vs && null (filter (\v -> price v <= t && speed v > speed v') vs)
  where
    price (Assignment3.Bicycle _ p) = p
    price (Assignment3.Car _ p _) = p
    price (Assignment3.Airplane _ p _) = p
    speed (Assignment3.Bicycle _ _) = -1 / 0 -- negative infinity
    speed (Assignment3.Car _ _ s) = s
    speed (Assignment3.Airplane _ _ s) = s

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

_ = Assignment3.append :: [a] -> [a] -> [a]
a3elem :: Eq a => a -> [a] -> Bool
a3elem = Assignment3.elem
_ = Assignment3.last :: [a] -> a
_ = Assignment3.reverse :: [a] -> [a]
_ = Assignment3.filter :: (a -> Bool) -> [a] -> [a]

prop_Exercise4_append xs ys = Assignment3.append xs ys == xs ++ (ys :: [Int])
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
  putStr "append: "
  quickCheck prop_Exercise4_append
  putStr "elem: "
  quickCheck prop_Exercise4_elem
  putStr "last: "
  quickCheck prop_Exercise4_last
  putStr "reverse: "
  quickCheck prop_Exercise4_reverse
  putStr "filter: "
  quickCheck prop_Exercise4_filter
