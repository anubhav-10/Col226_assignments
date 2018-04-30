type token =
  | VARS of (string)
  | CONST of (string)
  | COMMA
  | OPEN_PAR
  | CLOSE_PAR
  | OPEN_SQ
  | CLOSE_SQ
  | SPACE
  | RULE_SEP
  | SEMI_COLON
  | EOL
  | EOF
  | DOT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A6.program
val goal :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A6.goal
