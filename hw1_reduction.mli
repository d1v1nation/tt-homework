open Hw1
type lambdaRef = 
    | VarRef of string
    | AbsRef of string * lambdaRef ref
    | AppRef of lambdaRef ref * lambdaRef ref


(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
val free_to_subst: lambda -> lambda -> string -> bool (* 2 *)

(* Вернуть список имён свободных переменных *)
val free_vars: lambda -> string list (* 1 *)

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
val is_normal_form: lambda -> bool (* 3 *)

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
val is_alpha_equivalent: lambda -> lambda -> bool (* 4 *)

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
val normal_beta_reduction: lambda -> lambda (* 5 *)

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
val reduce_to_normal_form: lambda -> lambda
