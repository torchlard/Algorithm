module Main


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


main : IO ()
main = do putStrLn ""









