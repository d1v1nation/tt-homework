type token =
  | DOT
  | SPC
  | OP
  | CP
  | ABS
  | VAR of (string)
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.lambda
