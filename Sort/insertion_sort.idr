module Main

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs: List a) -> (x: a) -> ListLast (xs ++ [x])

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                        Empty => NonEmpty [] x
                        NonEmpty ys y => NonEmpty (x :: ys) y

sortHelp : Ord a => a -> List a -> List a
sortHelp a b with (listLast b)
  sortHelp b [] | Empty = [b]
  sortHelp b (xs ++ [x]) | NonEmpty xs x 
    = if b >= x then xs++[x,b]
      else sortHelp b xs ++ [x]

insertSort : Ord a => List a -> List a
insertSort xs = help [] xs where
  help : Ord a => (front: List a) -> (end: List a) -> List a
  help fs [] = fs
  help fs (x::xs)  = help (sortHelp x fs) xs




main : IO ()
main = do
  putStrLn $ show $ insertSort [3,44,38,5,47,15,36,26,27,2,46,4,19,50,48]









