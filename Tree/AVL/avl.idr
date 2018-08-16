module Main

data Tree : Type -> Type where
  Empty : Ord elem => Tree elem
  Node : Ord elem => (Tree elem) -> elem -> (Tree elem) -> Tree elem

-- rotate_left : Tree a -> Tree a
-- rotate_left (Node left val (Node a b c)) = Node (Node left val a) b c

-- rotate_right : Tree a -> Tree a
-- rotate_right (Node (Node a b c) val right) = Node a b (Node c val right)

depth : Tree a -> Int
depth Empty = 0
depth (Node left val right) = max (depth left +1) (depth right +1)

balance_factor : Tree a -> Tree a -> Int
balance_factor left right = depth left - depth right

getVal : Tree a -> a
getVal (Node a b c) = b
getL : Tree a -> Tree a
getL (Node a b c) = a
getR : Tree a -> Tree a
getR (Node a b c) = c


treeToList : Tree a-> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

balanceRR : Tree a -> Tree a
balanceRR (Node left val (Node a b c)) = (Node (Node left val a) b c)
balanceLL : Tree a -> Tree a
balanceLL (Node (Node a b c) val right) = (Node a b (Node c val right))
balanceLR : Tree a -> Tree a
balanceLR (Node (Node b v2 (Node c v3 d)) v1 a) = (Node (Node b v2 c) v3 (Node d v1 a))
balanceRL : Tree a -> Tree a
balanceRL (Node a v1 (Node (Node d v3 c) v2 b)) = (Node (Node a v1 d) v3 (Node c v2 b))

search : e -> Tree e -> Maybe e
search _ Empty = Nothing
search e (Node l v r) = case compare e v of
                          EQ => Just e
                          LT => search e l
                          GT => search e r

getHead : Tree e -> e
getHead Empty impossible
getHead e = head (treeToList e)                          

||| bug fix needed !!!
deletes : e -> Tree e -> Tree e
deletes e Empty = Empty
deletes e (Node Empty v Empty) = if e==v then Empty else (Node Empty v Empty)
deletes e (Node l v Empty) = if e==v then l else (Node l v Empty)
deletes e (Node Empty v r) = if e==v then r else (Node Empty v r)
deletes e (Node l v r) --mu = head (treeToList r)
  = let mu = getHead r
        dmin = deletes mu r  -- deletes in-order min element in right
        d_l = deletes e l  -- deletes element recursively in left side
        d_r = deletes e r in
      case compare e v of
          EQ => Node l mu dmin
          LT => if abs (balance_factor d_l r) < 2 then (Node d_l v r)  -- no imbalance, go ahead
                -- after deletion in left side, too high in right side, cause RR condition
                else if balance_factor (getL r) (getR r) < 0 then balanceRR (Node d_l v r)
                else balanceRL (Node d_l v r)  -- if not RR, then must be RL condition 
          GT => if abs (balance_factor l d_r) < 2 then (Node l v d_r)
                else if balance_factor (getL l) (getL l) > 0 then balanceLL (Node l v d_r)
                else balanceLR (Node l v d_r)
      -- where mu : Tree a -> a
      --       mu = head (treeToList r)

||| naive implementation of insert: match four cases recursively; calc depth each round
||| when detect inbalance, insert new node first and then balance the 3 elements
insert : elem -> Tree elem -> Tree elem
insert i Empty = Node Empty i Empty   -- recursive to the end of tree, actual insert operation
insert i (Node t v u)
  = let ti = insert i t   -- insert left
        ui = insert i u in  -- insert right
      case compare i v of   -- compare with middle val
        EQ => Node t v u   -- if equal, ignore insert
        LT => if (balance_factor ti u) == 2 then
                if (i < getVal t) then balanceLL (Node ti v u)
                                  else balanceLR (Node ti v u)
              else Node ti v u  -- insert to left subtree
              -- if navigate to end of tree, match one of four balance conditions
        GT => if (balance_factor t ui) == -2 then
  -- compare to val in right subtree (R); if insert to left of R, then cause RL condition
  -- nned to balanceRL, else insert to right of R, need balanceRR
                if i < getVal u then balanceRL (Node t v ui) 
                                else balanceRR (Node t v ui)
              else Node t v ui  -- insert to right subtree

              
-- ||| unbalanced insert              
-- insert : elem -> Tree elem -> Tree elem
-- insert i Empty = Node Empty i Empty
-- insert i (Node t v u)
--   = case compare i v of
--       EQ => Node t v u
--       LT => Node (insert i t) v u
--       GT => Node t v (insert i u)


addToTree : List a -> Tree a -> Tree a
addToTree [] tree = tree
addToTree (x :: xs) tree = addToTree xs (insert x tree)

-- balance : Tree a -> Tree a
-- balance Empty = Empty
-- balance (Node Empty _ Empty) = Node Empty _ Empty
-- balance @orig(Node left val right)
--   = case balance_factor orig of
--       <2 && >-2 => orig
--       >1 => balance (rotate_left orig)
--       <-1 => balance (rotate_right orig)


-- test : Int -> String
-- test a = if a > 1 then 
--             if a > 10 then "no1"
--                       else "no2"
--          else if a < -3 then "no2"
--          else "haha"

(Show k) => Show (Tree k) where
  show Empty = ""
  show (Node l v r) = unwords ["{" , show l , "(", show v, ")" , show r , "}"]


-- myRandom : Eff (List Integer) [RND, SYSTEM]
-- myRandom = do
--      srand !time
--      pure [ ! (rndInt 0 100), ! (rndInt 0 100), ! (rndInt 0 100)] 

main : IO ()
main = do 
  putStrLn "---------"
  -- putStrLn (show (treeToList (addToTree [5,4,7,3,6] Empty )))
  -- putStrLn (test 11)
  -- putStrLn (test 2)
  -- putStrLn (test (-8))






  