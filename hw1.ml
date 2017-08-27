open List;;
open String;;
open Lexer;;
open Parser;;
open Lambda;;
type lambda = Lambda.lambda;;
type peano = Z | S of peano;; 

let rec peano_of_int x = match x with
    0 -> Z
  | a -> S (peano_of_int (a - 1));;

let rec int_of_peano p = match p with
    Z -> 0
  | S x -> 1 + int_of_peano x;;
  
let isZ x = match x with
    Z -> true
  | _ -> false;;
  
let isNZ x = not (isZ x);;

let rec le ap = match ap with
    (x, Z) -> isZ x
  | (Z, y) -> true
  | (S x, S y) -> le (x, y);;
    
let dec x = match x with
    Z -> Z
  | S x -> x;;

let inc x = S x;;

  
let rec add x y = match y with
    Z -> x
  | S yp -> add (S x) yp;;
  
let rec mul x y = match y with
    Z -> Z
  | S yp -> add (mul x yp) x;;
  
let rec sub' ap = match ap with
    (Z, _) -> Z
  | (a, Z) -> a
  | (S a, S b) -> sub' (a, b);;

let rec sub x y = sub' (x, y);;
let rec sub x y = sub' (x, y);;
  
let rec div' x y m = if (le ((mul y m), x))
    then m
    else div' x y (dec m);;
    

let div x y = div' x y x;;

let rec power x y = match y with
    Z -> S Z
  | S yp -> mul x (power x yp)

let rec rev' acc l = match l with
    [] -> acc
  | (x :: xs) -> rev' (x :: acc) xs
                    
let rev x = rev' [] x;;

let rec take' ap = match ap with
    | (0, _) -> []
    | (n, (x :: xs)) -> x :: (take' ((n - 1), xs))
    | _ -> failwith "take fiasco";;
let take n l = take' (n, l);;

let rec drop' ap = match ap with
    | (0, x) -> x;
    | (n, (x :: xs)) -> drop' ((n - 1), xs)
    | _ -> failwith "drop fiasco";;
let drop n l = drop' (n, l);;

let rec len l = match l with
    | [] -> 0
    | (x :: xs) -> 1 + (len xs);;
    
let rec merge ap = match ap with
    | ([], r) -> r
    | (r, []) -> r
    | ((x :: xs), (y :: ys)) -> if (x < y) 
        then x :: (merge (xs, (y :: ys))) 
        else y :: (merge ((x :: xs), ys));;
    
let rec merge_sort x = match x with
    | [] -> []
    | el :: [] -> el :: []
    | list -> let hlen = (len list) / 2 in
        merge ((merge_sort (take hlen list)), (merge_sort (drop hlen list)));;
        
let head = take 1
let tail = drop 1
let fst = function
    | (x, _) -> x

let snd = function
    | (_, x) -> x

let rec string_of_lambda x = match x with
	| Var v -> v 
	| Abs (v, y) -> "" ^ "\\" ^ v ^ "." ^ (string_of_lambda y) ^ ""
	| App (l, r) -> "(" ^ (string_of_lambda l) ^ ") (" ^ (string_of_lambda r) ^ ")";;

let lambda_of_string x = Parser.main Lexer.token (Lexing.from_string (String.trim x));;
