type token =
  | NUMBER of (int)
  | VAR of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
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
  | RAISE
  | ERROR
  | TRY
  | EOF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t
