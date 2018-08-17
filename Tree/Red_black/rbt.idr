module Main

data Color = R | B 
data RB : Type -> Type where
  E : Ord a => RB a
  Tree : Ord a => Color -> (RB a) -> a -> (RB a) -> RB a


(Show k) => Show (RB k) where
  show E = ""
  show (Tree c l v r) = unwords ["{ [", conv c,"]", show l , "(", show v, ")" , show r , "}"]
    where conv B = "B"
          conv R = "R"

-- balance in condition of Red trees
balance : Ord a => RB a -> a -> RB a -> RB a
balance (Tree R a x b) y (Tree R c z d) = Tree R (Tree B a x b) y (Tree B c z d)  -- R v R
balance (Tree R (Tree R a x b) y c) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance (Tree R a x (Tree R b y c)) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance a x (Tree R b y (Tree R c z d)) = Tree R (Tree B a x b) y (Tree B c z d)
balance a x (Tree R (Tree R b y c) z d) = Tree R (Tree B a x b) y (Tree B c z d)
balance a x b = Tree B a x b

ins : Ord a => a -> RB a -> RB a
ins x E = Tree R E x E
ins x (Tree B a y b) = case compare x y of
        LT => balance (ins x a) y b   -- smaller -> go to left
        GT => balance a y (ins x b)   -- merge to right subtree
        EQ => Tree B a y b
ins x (Tree R a y b) = case compare x y of
      LT => Tree R (ins x a) y b  -- go left
      GT => Tree R a y (ins x b)
      EQ => Tree R a y b

RtoB : RB a -> RB a
RtoB (Tree _ a x b) = Tree B a x b
RtoB E = E

insert : Ord a => a -> RB a -> RB a
insert x s = RtoB (ins x s)


-- substitute color to R
sub1 : RB a -> RB a
sub1 (Tree B a x b) = Tree R a x b
sub1 E = E --error
      
balleft : RB a -> a -> RB a -> RB a
balleft (Tree R a x b) y c = Tree R (Tree B a x b) y c
balleft bl x (Tree B a y b) = balance bl x (Tree R a y b)
balleft bl x (Tree R (Tree B a y b) z c) = Tree R (Tree B bl x a) y (balance b z (sub1 c))

balright : RB a -> a -> RB a -> RB a
balright a x (Tree R b y c) = Tree R a x (Tree B b y c)
balright (Tree B a x b) y bl = balance (Tree R a x b) y bl
balright (Tree R a x (Tree B b y c)) z bl = Tree R (balance (sub1 a) x b) y (Tree B c z bl)  

app : RB a -> RB a ->RB a
app E x = x
app x E = x
app (Tree R a x b) (Tree R c y d) =
	case app b c of
	    Tree R b' z c' => Tree R(Tree R a x b') z (Tree R c' y d)
	    bc => Tree R a x (Tree R bc y d)
app (Tree B a x b) (Tree B c y d) = 
	case app b c of
	    Tree R b' z c' => Tree R(Tree B a x b') z (Tree B c' y d)
	    bc => balleft a x (Tree B bc y d)
app a (Tree R b x c) = Tree R (app a b) x c
app (Tree R a x b) c = Tree R a x (app b c)

mutual
  del : a -> RB a -> RB a
  del _ E = E
  del x (Tree _ a y b) = case compare x y of
    LT => delfromLeft x a y b 
    GT => delfromRight x a y b
    EQ => app a b

  delfromLeft : Ord a => a -> RB a -> a -> RB a -> RB a
  delfromLeft x k@(Tree B _ _ _) y b = balleft (del x k) y b
  delfromLeft x a y b = Tree R (del x a) y b

  delfromRight : Ord a => a -> RB a -> a -> RB a -> RB a
  delfromRight x a y b@(Tree B _ _ _) = balright a y (del x b)
  delfromRight x a y b = Tree R a y (del x b)

deletes: Ord a => a -> RB a -> RB a      
deletes x t = case del x t of
  Tree _ a y b => Tree B a y b
  otherwise => E

showing : Num a => RB a -> String
showing E = "e"  
showing _ = "fuck"

buildTree : Ord a => List a -> RB a -> RB a
buildTree [] tree = tree
buildTree (x::xs) tree = buildTree xs (insert x tree)


main : IO ()
main = do putStrLn ""








