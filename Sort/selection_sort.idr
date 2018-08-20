module Main

sortHelp : Ord a => (checked: List a) -> (init: List a) -> List a
sortHelp xs [] = xs
sortHelp [] (x::xs) = sortHelp [x] xs
sortHelp (y::ys) (x::xs) = if y<x then sortHelp ([y] ++ ys ++ [x]) xs
                           else sortHelp ([x] ++ ys ++ [y]) xs

selectSort : Ord a => List a -> List a
selectSort [] = []
selectSort xs = let sorted = sortHelp [] xs in
                take 1 sorted ++ selectSort (drop 1 sorted)



main : IO ()
main = do
  putStrLn $ show $ selectSort [3,44,38,5,47,15,36,26,27,2,46,4,19,50,48]


