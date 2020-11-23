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
  | REC
  | IN
  | FUN
  | ARROW
  | LBRACKET
  | RBRACKET
  | CONS
  | SEMI
  | MATCH
  | WITH
  | BAR
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
