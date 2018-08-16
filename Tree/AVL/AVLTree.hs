{-# LANGUAGE FlexibleInstances #-}

module AVLTree where

import Control.Applicative
import Test.QuickCheck
import Control.Arrow (second)

data Balance = MinusOne | Zero | PlusOne deriving (Eq, Show, Read)

instance Arbitrary Balance where
  arbitrary = elements [MinusOne, Zero, PlusOne]

data AVLTree k = Empty | Node k Balance (AVLTree k) (AVLTree k) deriving (Eq, Show, Read)

empty :: AVLTree k
empty = Empty

singleton :: k -> AVLTree k
singleton v = Node v Zero Empty Empty

-- Call to fix an inbalance of -2, returns True if height of root stayed
-- the same
rotateRight :: AVLTree k -> (AVLTree k, Bool)
rotateRight (Node u MinusOne (Node v MinusOne ta tb) tc)
  = (Node v Zero ta (Node u Zero tb tc), False)
rotateRight (Node u MinusOne (Node v Zero ta tb) tc)
  = (Node v PlusOne ta (Node u MinusOne tb tc), True)
rotateRight (Node u MinusOne (Node v PlusOne ta (Node w bw tb tc)) td)
  = let b1 = if bw == PlusOne  then MinusOne else Zero
        b2 = if bw == MinusOne then PlusOne  else Zero
    in (Node w Zero (Node v b1 ta tb) (Node u b2 tc td), False)
rotateRight _ = error "unexpected call of rotateRight"

-- Call to fix an inbalance of 2, returns True if height of root stayed
-- the same
rotateLeft :: AVLTree k -> (AVLTree k, Bool)
rotateLeft (Node u PlusOne tc (Node v PlusOne tb ta))
  = (Node v Zero (Node u Zero tc tb) ta, False)
rotateLeft (Node u PlusOne tc (Node v Zero tb ta))
  = (Node v MinusOne (Node u PlusOne tc tb) ta, True)
rotateLeft (Node u PlusOne td (Node v MinusOne (Node w bw tc tb) ta))
  = let b1 = if bw == PlusOne  then MinusOne else Zero
        b2 = if bw == MinusOne then PlusOne  else Zero
    in (Node w Zero (Node u b1 td tc) (Node v b2 tb ta), False)
rotateLeft _ = error "unexpected call of rotateLeft"

-- returns True if the height increased
insert' :: Ord k => k -> AVLTree k -> (AVLTree k, Bool)
insert' v Empty = (Node v Zero Empty Empty, True)
insert' v (Node w b tl tr) = case compare v w of
  EQ -> (Node w b tl tr, False)
  LT -> let (tl', isHigher) = insert' v tl in case (isHigher, b) of
    (False, _)       -> (Node w b tl' tr, False)
    (True, PlusOne)  -> (Node w Zero tl' tr, False)
    (True, Zero)     -> (Node w MinusOne tl' tr, True)
    (True, MinusOne) -> rotateRight (Node w MinusOne tl' tr)
  GT -> let (tr', isHigher) = insert' v tr in case (isHigher, b) of
    (False, _)       -> (Node w b tl tr', False)
    (True, MinusOne) -> (Node w Zero tl tr', False)
    (True, Zero)     -> (Node w PlusOne tl tr', True)
    (True, PlusOne)  -> rotateLeft (Node w PlusOne tl tr')

insert :: Ord k => k -> AVLTree k -> AVLTree k
insert v t = fst (insert' v t)

-- returns the maximum element and true if the height decreased
deleteMaximum :: Ord k => AVLTree k -> (AVLTree k, Bool, k)
deleteMaximum Empty = error "unexpected call of deleteMaximum"
deleteMaximum (Node v _ tl Empty) = (tl, True, v)
deleteMaximum (Node w b tl tr) =
  let (tr', isSmaller, v) = deleteMaximum tr in case (isSmaller, b) of
    (False, _)       -> (Node w b tl tr', False, v)
    (True, PlusOne)  -> (Node w Zero tl tr', True, v)
    (True, Zero)     -> (Node w MinusOne tl tr', False, v)
    (True, MinusOne) -> let (x, y) = rotateRight (Node w MinusOne tl tr')
                        in  (x, not y, v)
  

-- returns True if the height decreased
delete' :: Ord k => k -> AVLTree k -> (AVLTree k, Bool)
delete' v Empty = (Empty, False)
delete' v (Node w b tl tr) = case compare v w of
  EQ -> case tl of
    Empty -> (tr, True)
    _     -> let (tl', isSmaller, u) = deleteMaximum tl in case (isSmaller, b) of
      (False, _)       -> (Node u b tl' tr, False)
      (True, MinusOne) -> (Node u Zero tl' tr, True)
      (True, Zero)     -> (Node u PlusOne tl' tr, False)
      (True, PlusOne)  -> second not $ rotateLeft (Node u PlusOne tl' tr)
  LT -> let (tl', isSmaller) = delete' v tl in case (isSmaller, b) of
    (False, _)       -> (Node w b tl' tr, False)
    (True, MinusOne) -> (Node w Zero tl' tr, True)
    (True, Zero)     -> (Node w PlusOne tl' tr, False)
    (True, PlusOne)  -> second not $ rotateLeft (Node w PlusOne tl' tr)
  GT -> let (tr', isSmaller) = delete' v tr in case (isSmaller, b) of
    (False, _)       -> (Node w b tl tr', False)
    (True, PlusOne)  -> (Node w Zero tl tr', True)
    (True, Zero)     -> (Node w MinusOne tl tr', False)
    (True, MinusOne) -> second not $ rotateRight (Node w MinusOne tl tr')

delete :: Ord k => k -> AVLTree k -> AVLTree k
delete v t = fst (delete' v t)

hasKey :: Ord k => k -> AVLTree k -> Bool
hasKey _ Empty = False
hasKey v (Node w _ tl tr) = case compare v w of
  EQ -> True
  LT -> hasKey v tl
  GT -> hasKey v tr

toList :: AVLTree k -> [k]
toList Empty = []
toList (Node v _ tl tr) = toList tl ++ (v : toList tr)

inorder :: Ord k => [k] -> Bool
inorder []       = True
inorder [x]      = True
inorder (x:y:ys) = x < y && inorder (y:ys)

checkInorder :: Ord k => AVLTree k -> Bool
checkInorder = inorder . toList

height :: AVLTree k -> Int
height Empty = 0
height (Node _ _ tl tr) = succ (max (height tl) (height tr))

checkBalanced :: AVLTree k -> Bool
checkBalanced Empty = True
checkBalanced n@(Node _ b tl tr) = recl && recr && this
  where recl = checkBalanced tl
        recr = checkBalanced tr
        this = case (height tr - height tl) of
                 (-1) -> b == MinusOne
                 0    -> b == Zero
                 1    -> b == PlusOne
                 _    -> False

arbitraryOfHeight :: Int -> (Double, Double) -> Gen (AVLTree Double)
arbitraryOfHeight h bounds | h <= 0    = pure Empty
                           | h == 1    = Node <$> choose bounds <*> pure Zero
                                              <*> pure Empty    <*> pure Empty
                           | otherwise = do
  balance <- arbitrary
  v <- choose bounds
  let boundsLeft  = (fst bounds, v)
      boundsRight = (v, snd bounds)
  case balance of
    MinusOne -> Node v balance <$> arbitraryOfHeight (h-1) boundsLeft
                               <*> arbitraryOfHeight (h-2) boundsRight
    Zero     -> Node v balance <$> arbitraryOfHeight (h-1) boundsLeft
                               <*> arbitraryOfHeight (h-1) boundsRight
    PlusOne  -> Node v balance <$> arbitraryOfHeight (h-2) boundsLeft
                               <*> arbitraryOfHeight (h-1) boundsRight

data TestData = TestData { testTree :: AVLTree Double
                         , testKeys :: [Double] } deriving (Eq, Show, Read)

instance Arbitrary TestData where
  arbitrary = do
    t <- flip arbitraryOfHeight (-1000, 1000) =<< choose (0, 5)
    ks <- vectorOf 5 (choose (-1000, 1000))
    return $ TestData t ks

prop_insert_hasKey :: TestData -> Bool
prop_insert_hasKey (TestData t (k:_)) = hasKey k (insert k t)

executeInsertions :: TestData -> AVLTree Double
executeInsertions (TestData t ks) = foldr insert t ks

prop_insert_inorder :: TestData -> Bool
prop_insert_inorder = checkInorder . executeInsertions

prop_insert_balanced :: TestData -> Bool
prop_insert_balanced = checkBalanced . executeInsertions

prop_delete_hasKey :: TestData -> Bool
prop_delete_hasKey (TestData t (k:_)) = not $ hasKey k $ delete k $ insert k t

executeInsertionsDeletions :: TestData -> AVLTree Double
executeInsertionsDeletions (TestData t ks) = foldr delete (foldr insert t ks) ks

prop_delete_inorder :: TestData -> Bool
prop_delete_inorder = checkInorder . executeInsertionsDeletions

prop_delete_balanced :: TestData -> Bool
prop_delete_balanced = checkBalanced . executeInsertionsDeletions

main :: IO ()
main = do
  quickCheck prop_insert_hasKey
  quickCheck prop_insert_inorder
  quickCheck prop_insert_balanced
  quickCheck prop_delete_hasKey
  quickCheck prop_delete_inorder
  quickCheck prop_delete_balanced
  
  
  
  
  
  
  