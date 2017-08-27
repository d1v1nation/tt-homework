open List

type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let rec eqls t1 t2 = match (t1, t2) with
    | (Var x, Var y) -> x = y
    | (Fun (f, args1), Fun (g, args2)) -> f = g && List.for_all2 eqls args1 args2
    | _ -> false;;

let rec used var expr = match expr with
    | Var x -> x = var
    | Fun (f, args) -> List.exists (used var) args;;

let rec obtainFresh sys =
    let rec name term = 
        match term with
        | Var x -> x
        | Fun (f, args) -> f ^ (List.fold_left (^) "" (List.map name args))
    in 
        let names = List.map (fun (l, r) -> name l ^ name r) sys in
            "x" ^ (List.fold_left (^) "" names);;

let system_to_equation system = (* 1 *)
    let name = obtainFresh system and (lhs, rhs) = List.split system 
        in (Fun (name, lhs), Fun (name, rhs));;

let rec apply_substitution rules term = match term with (* 2 *)
    | Var x -> 
        (try let (v, e) = List.find (fun (var, expr) -> var = x) rules 
            in e 
            with Not_found -> term)
    | Fun (f, args) -> 
        Fun (f, List.map (fun arg -> apply_substitution rules arg) args);;


let rec check_solution subst system = (* 3 *)
    List.for_all 
        (fun (l, r) -> eqls (apply_substitution subst l) (apply_substitution subst r)) 
        system;;

exception SolveFail;;
let rec unify future past = match future with
    | [] -> 
        List.map 
            (fun (l, r) -> 
                match (l, r) with 
                | (Var x, _) -> (x, r) 
                | _ -> failwith "Something went wrong"
            ) past
    | (l, r) :: tail -> 
        if eqls l r then unify tail past else   match (l, r) with
            | (Fun (_, _), Var _) -> unify ((r, l) :: tail) past
            | (Fun (f, a1), Fun (g, a2)) -> 
                if f = g then 
                    (try let decomposed = List.combine a1 a2 in unify (decomposed @ tail) past
                    with Invalid_argument msg -> raise SolveFail)
                else raise SolveFail
            | (Var x, _) ->
                if used x r then raise SolveFail else
                let mapping = fun (a, b) -> (apply_substitution [(x, r)] a, apply_substitution [(x, r)] b) 
                    in unify (List.map mapping future) ((l, r) :: (List.map mapping past));;

let rec solve_system system = (try Some (unify system []) with SolveFail -> None);; (* 4 *)
