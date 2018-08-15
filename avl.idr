module Main

data Tree : Type -> Type where
  Empty : Ord elem => Tree elem
  Node : Ord elem => (Tree elem) -> elem -> (Tree elem) -> Tree elem

rotate_left : Tree a -> Tree a
rotate_left (Node left val (Node a b c)) = Node (Node left val a) b c

rotate_right : Tree a -> Tree a
rotate_right (Node (Node a b c) val right) = Node a b (Node c val right)

depth : Tree a -> Int
depth Empty = 0
depth (Node left val right) = max (depth left +1) (depth right +1)

balance_factor : Tree a -> Int
balance_factor (Node left _ right) = depth right - depth left



insert : elem -> Tree elem -> Tree elem
insert e Empty = Node Empty e Empty
-- insert e (Node l v r) 
--   = case compare e v of
--       EQ => Node l v r
--       LT => case balance_factor l 
--       (balance_factor l r) == 2 && e < getM v => balanceLL (Node l v r)
--       LT && (balance_factor )

treeToList : Tree a-> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

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

balanceLL : Tree a -> Tree a
balanceLL (Node left val (Node a b c)) = Node (Node left val a) b c
balanceRR : Tree a -> Tree a
balanceRR (Node (Node a b c) val right) = Node a b (Node c val right)
balanceLR : Tree a -> Tree a
balanceLR (Node (Node b v2 (Node c v3 d)) v1 a) = Node (Node b v2 c) v3 (Node d v1 a)
balanceRL : Tree a -> Tree a
balanceRL (Node a v1 (Node (Node d v3 c) v2 b)) = Node (Node a v1 d) v3 (Node c v2 b)


test : Int -> String
-- test a = case compare a 1 of
--           GT => if (a > 10) then case a>5 of
--                   True => "true"
--                   False => "false";
--           LT => "small"
--           EQ => "mid"
--           _  => "others"
test a = if (a > 1) then "no"
         else if (a < 1) then 


main : IO ()
main = do 
  putStrLn "---------"
  -- putStrLn (show (treeToList (addToTree [5,4,7,3,6] Empty )))
  putStrLn (test 3)









  