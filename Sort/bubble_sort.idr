module Main

sortHelp : Ord a => (checked: List a) -> (init: List a) -> List a
sortHelp xs [x] = xs ++ [x]
sortHelp ys (x::y::xs) =  if x<=y then sortHelp (ys ++ [x]) (y::xs)
                          else sortHelp (ys ++ [y]) (x::xs)

-- bubbleSort : Ord a => List a -> List a
-- bubbleSort [] = []
-- bubbleSort xs = let sorted = sortHelp [] xs in
--                 take 1 sorted ++ selectSort (drop 1 sorted)



main : IO ()
main = do
  putStrLn ""
  -- putStrLn $ show $ bubbleSort [3,44,38,5,47,15,36,26,27,2,46,4,19,50,48]


