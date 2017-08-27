{
	open Parser
}

rule token = parse
	| "\\" [' ']* 						{ ABS }
	| ['a'-'z']+ ['0'-'9']* as name		{ VAR name }
	| '(' [' ']* 						{ OP }
	| [' ']* ')'						{ CP }
	| [' ']* '.' [' ']*					{ DOT }
	| [' ']+							{ SPC }
| eof { EOF }
