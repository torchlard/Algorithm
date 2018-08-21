module Main

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs: List a) -> (x: a) -> ListLast (xs ++ [x])

listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                        Empty => NonEmpty [] x
                        NonEmpty ys y => NonEmpty (x :: ys) y


sortHelp : Ord a => (checked: List a) -> (init: List a) -> List a
sortHelp xs [x] = xs ++ [x]
sortHelp ys (x::y::xs) =  if x<=y then sortHelp (ys ++ [x]) (y::xs)
                          else sortHelp (ys ++ [y]) (x::xs)

bubbleSort : Ord a => List a -> List a
bubbleSort [] = []
bubbleSort xs = help $ sortHelp [] xs where
  help : Ord a => List a -> List a
  help xs with (listLast xs)
    help [] | Empty = []
    help (as++[x]) | NonEmpty as x = (bubbleSort as) ++ [x]

      



main : IO ()
main = do
  -- putStrLn ""
  putStrLn $ show $ bubbleSort [4]
  putStrLn $ show $ bubbleSort [3,44,38,5,47,15,36,26,27,2,46,4,19,50,48]


