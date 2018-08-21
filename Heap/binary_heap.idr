module Main

-- min heap
data BHeap = Type -> Type
  E : Ord a => BHeap a
  Node : Ord a => BHeap a -> a -> BHeap a -> BHeap a

-- initH : Ord a => List a -> BHeap a
-- initH = E

insert : (Ord a, Nat n) => a -> (n, BHeap a) -> (n, BHeap a)
  help 0 a (n, E) = (S n, Node E a E)
  help k a (n, Node l v r) = if n `mod` 2 == 0 then help (k*2) 




main : IO ()
main = do
  putStrLn ""










