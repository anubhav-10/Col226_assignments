type token = 
	| VARS of (string)
	| CONST of (string)
	| COMMA
	| OPEN_PAR
	| CLOSE_PAR
	| OPEN_SQU
	| CLOSE_SQU
	| SPACE
	| RULE_SEP
    | SEMI_COLON 
	| EOL
	| EOF
	| DOT

val main:
	(Lexing.lexbuf -> token) -> Lexing.lexbuf -> A6.program

val goal:
	(Lexing.lexbuf -> token) -> Lexing.lexbuf -> A6.goal
