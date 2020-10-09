type token =
  | NUMBER of (int)
  | PLUS
  | MINUS
  | TIMES
  | LPAREN
  | RPAREN
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
