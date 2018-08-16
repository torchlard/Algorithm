module Main

data Color = R | B
data RB : Type -> Type where
  E : Ord a => RB a
  Tree : Ord a => Color -> (RB a) -> a -> (RB a) -> RB a

-- balance in condition of Red trees
balance : Ord a => RB a -> a -> RB a -> RB a
balance (Tree R a x b) y (Tree R c z d) = Tree R (Tree B a x b) y (Tree B c z d)  -- R v R
balance (Tree R (Tree R a x b) y c) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance (Tree R a x (Tree R b y c)) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance a x (Tree R b y (Tree R c z d)) = Tree R (Tree B a x b) y (Tree B c z d)
balance a x (Tree R (Tree R b y c) z d) = Tree R (Tree B a x b) y (Tree B c z d)
balance a x b = Tree B a x b


insert : Ord a => a -> RB a -> RB a
insert x E = Tree R E x E
insert x (Tree c z a b) = ins (Tree c z a b) where
    -- ins : RB -> RB
    ins s@(Tree B a y b) = case compare x y of
      LT => balance (ins a) y b   -- smaller -> go to left
      GT => balance a y (ins b)   -- merge to right subtree
      EQ => s
    ins s@(Tree R a y b) = case compare x y of
      LT => Tree R (ins a) y b  -- go left
      GT => Tree R a y (ins b)
      EQ => s
  

(Show k) => Show (RB k) where
  show E = ""
  show (Tree c l v r) = unwords ["{ [", conv c,"]", show l , "(", show v, ")" , show r , "}"]
    where conv B = "B"
          conv R = "R"


buildTree : Ord a => List a -> RB a -> RB a
buildTree [] tree = tree
buildTree [x] tree = insert x tree
buildTree (x::xs) tree = insert x (buildTree xs tree)


main : IO ()
main = do putStrLn ""


