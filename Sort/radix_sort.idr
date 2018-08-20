module Main
import Data.Vect


initVect : Vect 10 (List String)
initVect = replicate 10 []

getMaxLength : List Int -> Nat
getMaxLength xs = Prelude.Strings.length (show $ foldr max 0 xs)

convertStr : List Int -> List String
convertStr xs = map (leftPad 4 . show) xs where                      
  leftPad : Nat -> String -> String
  leftPad n str = if length str >= n then str
                  else "0" ++ leftPad (pred n) str

charToFin : Int -> Fin 10
charToFin d = case d of 
   1=>1;2=>2; 3=>3;4=>4;5=>5;6=>6;7=>7;8=>8;9=>9;_=>0;

-- digits : (digit: Int) -> (target: List String) -> Vect 10 (List String) -> Vect 10 (List String)
-- digits i [] base = base
-- digits i (x::xs) base = let dig = strIndex x i in   -- get digit at string
--                         updateAt (charToFin dig) (++ [x]) $ digits i xs base

intToFinList : Int -> List (Fin 10)
intToFinList n = if n<10 then [charToFin n]
                 else intToFinList (n `div` 10) ++ [charToFin (n - n `div` 10 *10)]


parseMaybe : Maybe Integer -> Integer
parseMaybe (Just a) = a
parseMaybe Nothing = 0

-- radix_sort : List Int -> Vect 10 (List String)
-- radix_sort xs = digits 4 (convertStr xs) initVect


main : IO ()
main = do 
  putStrLn ""
  
  
-- [3221, 1, 10, 9680, 577, 9420, 7, 5622, 4793, 2030, 3138, 82, 2599, 743, 4127]
