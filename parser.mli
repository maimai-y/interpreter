type token =
  | NUMBER of (int)
  | VAR of (string)
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
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | ARROW
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
