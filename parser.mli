type token =
  | NUMBER of (int)
  | PLUS
  | MINUS
  | TIMES
  | TRUE
  | FALSE
  | EQUAL
  | LESS
  | GREATER
  | LPAREN
  | RPAREN
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
