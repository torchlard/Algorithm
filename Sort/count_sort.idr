module Main
import Data.Vect


initVect : (n: Nat) -> Vect n Nat
initVect n = replicate n 0

update : Fin n -> Vect n Nat -> Vect n Nat
update i xs = updateAt i (+1) xs

counting : List (Fin n) -> Vect n Nat -> Vect n Nat
counting [] ys = ys
counting (x::xs) ys = update x $ counting xs ys

display : Vect n Nat -> List Nat
display xs = helper xs 0 where
    helper : Vect n Nat -> Nat -> List Nat
    helper [] _ = []
    helper (x::xs) i = (replicate x i) ++ (helper xs (i+1))
    
countSort : (n: Nat) -> List (Fin n) -> List Nat    
countSort n xs = display $ counting xs $ initVect n


main : IO ()
main = do 
  putStrLn $ show $ countSort 10 [2,3,8,7,1,2,2,2,7,3,9,8,2,1,4,2] 
  
  

