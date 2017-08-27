open List
open Map
open Lambda
open Hw1
type lambdaRef = 
    | VarRef of string
    | AbsRef of string * lambdaRef ref
    | AppRef of lambdaRef ref * lambdaRef ref
module SM = Map.Make(String);;

let fname = ref "m";;
let id = ref 0;;

let nname () = let name = (!fname) ^ (string_of_int (!id)) in id := !id + 1; name;;

let rec obtainFresh expr = match expr with
    | Var x -> x ^ x
    | App (p, q) -> (obtainFresh p) ^ (obtainFresh q)
    | Abs (x, p) -> x ^ (obtainFresh p);;

let rec freeVars' expr free except = match expr with
    | Var x -> 
        if not (List.mem x free) && not (List.mem x except) 
        then x :: free 
        else free
    | App (p, q) -> freeVars' p (freeVars' q free except) except
    | Abs (x, p) -> freeVars' p free (x :: except);;

let rec rename' expr vars = match expr with
    | Var x -> (try SM.find x vars with Not_found -> expr)
    | App (p, q) -> App (rename' p vars, rename' q vars)
    | Abs (x, p) -> let name = nname() in Abs (name, rename' p (SM.add x (Var name) vars));;

let rename expr = rename' expr SM.empty;;

let rec copy' expr vars = match !expr with
    | VarRef x -> (try ref (SM.find x vars) with _ -> ref (VarRef x))
    | AppRef (p, q) -> ref (AppRef (copy' p vars, copy' q vars))
    | AbsRef (x, p) -> 
        let name = nname() in ref (AbsRef (name, copy' p (SM.add x (VarRef name) vars)));;

let copy expr = copy' expr SM.empty;;

let rec fromRef expr = match !expr with
    | VarRef x -> Var x
    | AppRef (p, q) -> App (fromRef p, fromRef q)
    | AbsRef (x, p) -> Abs (x, fromRef p);;

let rec toRef expr = match expr with
    | Var x -> ref (VarRef x)
    | App (p, q) -> ref (AppRef (toRef p, toRef q))
    | Abs (x, p) -> ref (AbsRef (x, toRef p));;

let rec subst what expr var = match expr with
    | Var x -> if x = var then what else expr
    | App (p, q) -> App (subst what p var, subst what q var)
    | Abs (x, p) -> if x = var then expr else Abs (x, subst what p var);;

let rec substLRef what expr var = match !expr with
    | VarRef x -> if x = var then expr := !what
    | AbsRef (x, p) -> if x <> var then substLRef what p var
    | AppRef (p, q) -> substLRef what p var; substLRef what q var;;

let rec memoReduce expr = match !expr with
    | VarRef _ -> false
    | AbsRef (x, p) -> memoReduce p
    | AppRef (p, q) -> 
        match !p with
            | AbsRef(x, m) -> expr := !(copy m); substLRef q expr x; true
            | _ -> memoReduce p || memoReduce q;;

let rec isNF expr = match expr with
    | Var _ -> true
    | App (Abs (_, _), _) -> false
    | App (p, q) -> isNF p && isNF q
    | Abs (_, p) -> isNF p;;


let rec reduceNF expr = if isNF expr then expr 
    else match expr with
        | Var _ -> expr
        | App (Abs (x, p), q) -> subst q p x
        | App (p, q) -> if isNF p then App (p, reduceNF q) else App (reduceNF p, q)
        | Abs (x, p) -> Abs (x, reduceNF p);;

(* PUBLIC impl *)

let free_vars expr = freeVars' expr [] [];; (* 2 *)

let rec free_to_subst what expr var = match expr with (* 1 *)
    | Var _ -> true
    | App (p, q) -> free_to_subst what p var && free_to_subst what q var
    | Abs (x, p) -> x = var || (not (List.mem x (free_vars what)) && free_to_subst what p var);;


let is_normal_form = isNF (* 3 *)

let rec is_alpha_equivalent l1 l2 = match l1, l2 with (* 4 *)
    | Var x1, Var x2 -> x1 = x2
    | App (p1, q1), App (p2, q2) -> is_alpha_equivalent p1 p2 && is_alpha_equivalent q1 q2
    | Abs (x1, p1), Abs (x2, p2) -> 
        let t = Var (nname()) in
        is_alpha_equivalent (subst t p1 x1) (subst t p2 x2)
    | _ -> false;;

let normal_beta_reduction expr = reduceNF (rename expr);; (* 5 *)

let reduce_to_normal_form expr =  (* 6 *)
    let exprRef = copy (toRef expr) 
        in while memoReduce exprRef do () done; fromRef exprRef;;
