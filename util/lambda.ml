type lambda = 
	| Var of string 
	| Abs of string * lambda 
    | App of lambda * lambda

type lambdaRef = 
	| VarRef of string
	| AbsRef of string * lambdaRef ref
	| AppRef of lambdaRef ref * lambdaRef ref
