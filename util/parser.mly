%token DOT
%token SPC
%token OP
%token CP
%token ABS
%token <string> VAR
%token EOF

%nonassoc DOT
%left SPC

%start main
%type <Lambda.lambda> main
%%

main:
	| parse EOF	{ $1 }
;

parse:
	| OP parse CP 	            { $2 }
	| VAR					    { Lambda.Var $1 }
    | ABS VAR DOT parse         { Lambda.Abs ($2, $4) }
	| parse SPC parse	  	    { Lambda.App ($1, $3) }
;
