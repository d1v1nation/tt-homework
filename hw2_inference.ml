open List
open Lambda
open Hw1
open Hw2_unify

type simp_type = 
    | S_Elem of string 
    | S_Arrow of simp_type * simp_type

type hm_lambda = 
    | HM_Var of string 
    | HM_Abs of string * hm_lambda 
    | HM_App of hm_lambda * hm_lambda 
    | HM_Let of string * hm_lambda * hm_lambda

type hm_type = 
    | HM_Elem of string 
    | HM_Arrow of hm_type * hm_type 
    | HM_ForAll of string * hm_type
    
module SM = Map.Make(String);;
exception SolveFail;;
let union m1 m2 = SM.fold (fun k v m -> SM.add k v m) m1 m2;;
let bottom = "_|_"
let counter = ref 0;;
let obtainName () = let name = "_N" ^ (string_of_int (!counter)) in counter := !counter + 1; name;;
let obtainType () = let name = "_T" ^ (string_of_int (!counter)) in counter := !counter + 1; Var name;;
let obtainHMType () = let name = "_T" ^ (string_of_int (!counter)) in counter := !counter + 1; HM_Elem name;;

let rec fromHM expr = match expr with
    | HM_Elem x -> Var x
    | HM_Arrow (t1, t2) -> Fun ("_A", [fromHM t1; fromHM t2])
    | HM_ForAll (x, t) -> Fun ("_F", [Var x; fromHM t]);;

let rec toHM term = match term with
    | Var x -> HM_Elem x
    | Fun (f, l :: r :: []) when f = "_A" -> HM_Arrow (toHM l, toHM r)
    | Fun (f, Var x :: r :: []) when f = "_F" -> HM_ForAll (x, toHM r)
    | _ -> failwith bottom;;
    
let rec free' expr free except = match expr with
    | HM_Elem x -> if not (List.mem x except || List.mem x free) then x :: free else free
    | HM_Arrow (p, q) -> (free' p free except) @ (free' q free except)
    | HM_ForAll (x, p) -> free' p free (x :: except);;
let free expr = free' expr [] [];;

let freeContext context = SM.fold (fun k v l -> (free v) @ l) context [];;

let rec abstract context t = let fv_t = free t and fv_c = freeContext context 
    in List.fold_left (fun t v -> if List.mem v fv_c then t else HM_ForAll (v, t)) t fv_t;;
    
let rec freeTyped expr free except = match expr with
    | HM_Var x -> 
        if not (List.mem x except || SM.mem x free) 
        then SM.add x (obtainHMType ()) free else free
    | HM_Abs (x, p) -> freeTyped p free (x :: except)
    | HM_App (p, q) -> union (freeTyped p free except) (freeTyped q free except)
    | HM_Let (x, p, q) -> union (freeTyped p free except) (freeTyped q free (x :: except));;

let contextualize expr = freeTyped expr SM.empty [];;

let rec prune' lst res = match lst with
    | [] -> res
    | h :: tail -> if List.mem h res then prune' tail res else prune' tail (h :: res);;
let rec prune lst = prune' lst [];;

let rec typify term = match term with
    | Var x -> S_Elem x
    | Fun ("__fun_", l :: r :: []) -> S_Arrow (typify l, typify r)
    | _ -> failwith bottom;;

let rec subst expr vars = match expr with
    | HM_Elem t -> (try SM.find t vars with Not_found -> expr)
    | HM_Arrow (t1, t2) -> HM_Arrow (subst t1 vars, subst t2 vars)
    | HM_ForAll (_, _) -> failwith bottom;;

let rec rename expr vars = match expr with
    | HM_ForAll (x, t) -> rename t (SM.add x (obtainHMType ()) vars)
    | _ -> subst expr vars;;

let substFresh expr = rename expr SM.empty;;

let rec rename' expr vars = match expr with
    | HM_Var x -> (try SM.find x vars with Not_found -> expr)
    | HM_Abs (x, p) -> let v = obtainName () in HM_Abs (v, rename' p (SM.add x (HM_Var v) vars))
    | HM_App (p, q) -> HM_App (rename' p vars, rename' q vars)
    | HM_Let (x, p, q) -> let v = obtainName () 
        in let vs = SM.add x (HM_Var v) vars 
            in HM_Let (v, rename' p vs, rename' q vs);;
let rename expr = rename' expr SM.empty;;

let rec substHM subst t = match t with
    | HM_Elem x -> (try let (v, e) = List.find (fun (var, expr) -> var = x) subst in e with Not_found -> t) 
    | HM_Arrow (t1, t2) -> HM_Arrow (substHM subst t1, substHM subst t2)
    | HM_ForAll (x, t1) -> HM_ForAll (x, substHM (List.filter (fun (v, e) -> v <> x) subst) t1);;

let rec makeSys' context expr goal equations = match expr with
    | Lambda.Var x -> 
        (try (context, (let (_, e) = List.find (fun (t, e) -> t = x) context in e, goal) :: equations)
        with Not_found -> failwith bottom)
    | Lambda.Abs (x, p) -> 
        let x_type = obtainType () and p_type = obtainType () 
            in makeSys' ((x, x_type) :: context) p p_type ((goal, Fun ("__fun_", [x_type; p_type])) :: equations) 
    | Lambda.App (p, q) -> 
        let q_type = obtainType () in let p_type = Fun ("__fun_", [q_type; goal]) 
            in let (c1, e1) = (makeSys' context p p_type equations) 
                and (c2, e2) = (makeSys' context q q_type equations) 
                in (c1 @ c2, e1 @ e2);;
let makeSys expr = let (context, system) = makeSys' [] expr (obtainType()) [] 
    in (prune context, prune system);;

let mapToContext subst context = SM.map (fun t -> substHM subst t) context;;
    
let compose' s1 s2 = 
    let substed = List.map (fun (x, t) -> (x, substHM s1 t)) s2 
        in let filtered = List.filter (fun (x1, t1) -> 
            (try let _ = List.find (fun (x2, t2) -> x1 = x2) s1 in false with Not_found -> true)) substed 
                in s1 @ filtered;;
let rec compose subst_list = match subst_list with 
    | [] -> []
    | h :: [] -> h
    | h :: tail -> compose' h (compose tail);;

let rec inferHM context expr = match expr with
    | HM_Var x -> (try ([], substFresh (SM.find x context)) with Not_found -> raise SolveFail)
    | HM_Abs (x, e) -> 
        let beta = obtainHMType () 
            in let (s, t) = inferHM (SM.add x beta context) e 
                in (s, HM_Arrow (substHM s beta, t))
    | HM_App (e1, e2) -> 
        let (s1, t1) = inferHM context e1 
            in let (s2, t2) = inferHM (mapToContext s1 context) e2 and beta = obtainHMType () 
                in (match solve_system [(fromHM (substHM s2 t1), fromHM (HM_Arrow (t2, beta)))] with
                    | Some v -> let s = List.map (fun (x, t) -> (x, toHM t)) v 
                        in (compose [s; s2; s1], substHM s beta)
                    | None -> raise SolveFail)
    | HM_Let (x, e1, e2) -> 
        let (s1, t1) = inferHM context e1 in let new_context = mapToContext s1 context 
            in let (s2, t2) = inferHM (SM.add x (abstract new_context t1) new_context) e2 
                in (compose [s2; s1], t2);;

let infer_simp_type expr = 
    counter := 0; 
    let (context, sys) = makeSys expr 
    in match solve_system sys with
        | Some solution -> Some (
            List.map 
                (fun (x, t) -> (x, typify t)) 
                (List.map (fun (x, t) -> (x, apply_substitution solution t)) context), 
            let (_, tmp) = List.find (fun (x, t) -> x = "_T" ^ "0") solution 
                in typify tmp
            )
        | None -> None;;

let algorithm_w expr = counter := 0; 
    let e = rename expr 
        in (try Some (inferHM (contextualize e) e) with SolveFail -> None);;
