module Main

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs: List a) -> (x: a) -> ListLast (xs ++ [x])

-- covering function of ListLast view
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                        Empty => NonEmpty [] x
                        NonEmpty ys y => NonEmpty (x :: ys) y


mergeSort : Ord a => (List a , List a)  -> List a
mergeSort ( x, []) = x
mergeSort ([], x) = x
mergeSort ((x::xs), (y::ys)) = if x<y then x :: mergeSort ( xs, y::ys )
                                      else y :: mergeSort ( x::xs, ys )

merges : Ord a => List a -> List a
merges [] = []
merges x = let len = length x 
               pair = splitAt (len `div` 2) x 
               s1 = fst pair ; s2 = snd pair in
           if len<= 2 then mergeSort (s1, s2)
           else mergeSort (merges s1, merges s2)


insertCmp : Ord a => a -> List a -> List a
insertCmp a b with (listLast b)
    insertCmp a [] | Empty = [a]
    insertCmp a (xs ++ [x]) | NonEmpty xs x 
        = if a < x then (insertCmp a xs) ++ [x]
                   else xs ++ [x] ++ [a]
                   
insertSort : Ord a => List a -> List a
insertSort [] = []
insertSort (x::xs) = insHelp [] x xs where
    insHelp : Ord a => List a -> a -> List a -> List a
    insHelp [] x [] = [x]
    insHelp ls x [] = insertCmp x ls
    insHelp ls x (y::rs) = insHelp (insertCmp x ls) y rs

    
-- quickCmp : Ord a => (pivot: a) -> (left: List a) -> (right: List a) -> (iter: List a) 
--                     -> List a
-- quickCmp p [] [] [] = [p]     -- only 1 element left
-- quickCmp p [] (r::rs) [] = [p] ++ (quickCmp r [] [] rs)
-- quickCmp p (l::ls) [] [] = (quickCmp l [] [] ls) ++ [p]
-- quickCmp p (l::ls) (r::rs) [] = (quickCmp l [] [] ls) ++ [p] ++ (quickCmp r [] [] rs)
-- quickCmp p ls rs (x::xs) = if x < p then (quickCmp p (ls++[x]) rs xs)
--                            else quickCmp p ls (rs++[x]) xs

-- quickSort : Ord a => List a -> List a
-- quickSort [] = []                           
-- quickSort (x::xs) = quickCmp x [] [] xs

quickSort : Ord a => List a -> List a
quickSort [] = []
quickSort (x::xs) = quickSort [a |a<-xs, a<x] ++ [x] ++ quickSort [a | a<-xs, a>=x]







main : IO ()
main = do putStrLn ""









