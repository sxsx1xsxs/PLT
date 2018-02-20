type token =
  | SEMI
  | COMMA
  | LPR
  | RPR
  | LBK
  | RBK
  | LBC
  | RBC
  | IF
  | ELIF
  | ELSE
  | FOR
  | WHILE
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EQ
  | NEQ
  | LEQ
  | REQ
  | RAPPEND
  | LAPPEND
  | LR
  | RL
  | AND
  | OR
  | NOT
  | REGEX
  | EOF
  | RETURN
  | BOOL
  | VOID
  | FLOAT
  | STRING
  | INT
  | ID of (string)
  | REGEX_STRING of (string)
  | STRING_T of (string)
  | INT_T of (int)
  | FLOAT_T of (float)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
