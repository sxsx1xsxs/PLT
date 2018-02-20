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

open Parsing;;
# 6 "parser.mly"
open Ast
let quote_remover a = String.sub a 1 ((String.length a) - 2);;
# 52 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* COMMA *);
  259 (* LPR *);
  260 (* RPR *);
  261 (* LBK *);
  262 (* RBK *);
  263 (* LBC *);
  264 (* RBC *);
  265 (* IF *);
  266 (* ELIF *);
  267 (* ELSE *);
  268 (* FOR *);
  269 (* WHILE *);
  270 (* TRUE *);
  271 (* FALSE *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* TIMES *);
  275 (* DIVIDE *);
  276 (* MOD *);
  277 (* EQ *);
  278 (* NEQ *);
  279 (* LEQ *);
  280 (* REQ *);
  281 (* RAPPEND *);
  282 (* LAPPEND *);
  283 (* LR *);
  284 (* RL *);
  285 (* AND *);
  286 (* OR *);
  287 (* NOT *);
  288 (* REGEX *);
    0 (* EOF *);
  289 (* RETURN *);
  290 (* BOOL *);
  291 (* VOID *);
  292 (* FLOAT *);
  293 (* STRING *);
  294 (* INT *);
    0|]

let yytransl_block = [|
  295 (* ID *);
  296 (* REGEX_STRING *);
  297 (* STRING_T *);
  298 (* INT_T *);
  299 (* FLOAT_T *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\009\000\009\000\007\000\007\000\010\000\
\011\000\011\000\003\000\003\000\013\000\014\000\015\000\008\000\
\008\000\017\000\017\000\017\000\017\000\017\000\017\000\018\000\
\018\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\019\000\
\019\000\020\000\020\000\016\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\009\000\001\000\001\000\001\000\001\000\
\001\000\000\000\001\000\002\000\004\000\000\000\002\000\004\000\
\000\000\002\000\000\000\002\000\002\000\003\000\004\000\000\000\
\002\000\002\000\003\000\003\000\007\000\009\000\005\000\000\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\003\000\002\000\004\000\003\000\000\000\
\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\061\000\000\000\008\000\005\000\007\000\006\000\
\009\000\000\000\003\000\000\000\000\000\001\000\020\000\000\000\
\000\000\060\000\000\000\014\000\021\000\000\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\012\000\000\000\
\000\000\000\000\000\000\000\000\024\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\036\000\035\000\
\000\000\025\000\014\000\000\000\000\000\000\000\000\000\053\000\
\000\000\000\000\000\000\000\000\000\000\051\000\000\000\000\000\
\000\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\016\000\055\000\028\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\054\000\000\000\004\000\
\000\000\000\000\031\000\000\000\000\000\000\000\029\000\000\000\
\000\000\030\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\028\000\026\000\024\000\029\000\
\027\000\030\000\054\000\049\000\015\000\016\000\021\000\019\000\
\050\000\087\000\091\000\092\000"

let yysindex = "\020\000\
\000\000\000\000\000\000\232\254\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\233\254\242\254\000\000\000\000\020\255\
\041\255\000\000\024\255\000\000\000\000\232\254\000\000\232\254\
\011\255\053\255\059\255\033\255\046\255\000\000\000\000\068\255\
\232\254\021\255\159\255\072\255\000\000\000\000\071\255\077\255\
\079\255\159\255\159\255\159\255\000\255\000\000\000\000\000\000\
\206\255\000\000\000\000\045\255\159\255\085\255\190\000\000\000\
\087\255\159\255\159\255\159\255\243\254\000\000\225\255\159\255\
\159\255\000\000\159\255\159\255\159\255\159\255\159\255\159\255\
\159\255\159\255\159\255\159\255\159\255\159\255\232\254\000\000\
\237\000\000\000\000\000\000\000\206\000\237\000\090\255\222\000\
\000\000\237\000\093\255\096\255\237\000\243\254\243\254\000\000\
\000\000\009\001\009\001\202\255\202\255\202\255\202\255\252\000\
\040\000\100\255\154\255\159\255\154\255\000\000\159\255\000\000\
\091\255\244\255\000\000\237\000\154\255\159\255\000\000\097\255\
\154\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\102\255\000\000\141\255\
\000\000\000\000\106\255\000\000\000\000\000\000\000\000\000\000\
\000\000\110\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\187\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\113\255\000\000\019\000\000\000\000\000\111\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\141\255\000\000\
\115\255\000\000\000\000\000\000\000\000\027\255\000\000\000\000\
\000\000\013\255\000\000\117\255\067\255\049\000\079\000\000\000\
\000\000\163\000\169\000\089\000\119\000\129\000\159\000\018\255\
\063\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\058\255\000\000\053\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\048\000\000\000\068\000\223\255\
\000\000\000\000\000\000\221\255\000\000\000\000\000\000\000\000\
\015\000\005\000\000\000\000\000"

let yytablesize = 549
let yytable = "\055\000\
\014\000\019\000\064\000\057\000\069\000\070\000\061\000\062\000\
\063\000\005\000\006\000\007\000\008\000\009\000\058\000\017\000\
\058\000\081\000\048\000\048\000\001\000\048\000\085\000\086\000\
\088\000\018\000\020\000\033\000\090\000\093\000\033\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\101\000\102\000\
\103\000\104\000\105\000\022\000\065\000\106\000\048\000\048\000\
\035\000\031\000\036\000\012\000\037\000\038\000\039\000\023\000\
\032\000\040\000\041\000\059\000\033\000\059\000\042\000\049\000\
\049\000\053\000\049\000\052\000\052\000\025\000\052\000\034\000\
\114\000\058\000\051\000\116\000\043\000\056\000\044\000\059\000\
\052\000\060\000\086\000\080\000\045\000\082\000\046\000\047\000\
\048\000\035\000\108\000\036\000\049\000\037\000\084\000\039\000\
\110\000\111\000\040\000\041\000\121\000\117\000\035\000\042\000\
\036\000\010\000\037\000\112\000\039\000\011\000\017\000\040\000\
\041\000\032\000\056\000\018\000\042\000\043\000\079\000\044\000\
\057\000\113\000\120\000\115\000\000\000\045\000\000\000\046\000\
\047\000\048\000\043\000\119\000\044\000\000\000\000\000\122\000\
\000\000\000\000\045\000\000\000\046\000\047\000\048\000\024\000\
\000\000\024\000\000\000\024\000\024\000\024\000\000\000\000\000\
\024\000\024\000\000\000\000\000\035\000\024\000\036\000\000\000\
\037\000\035\000\039\000\036\000\000\000\040\000\041\000\000\000\
\000\000\000\000\042\000\024\000\000\000\024\000\000\000\042\000\
\000\000\000\000\000\000\024\000\000\000\024\000\024\000\024\000\
\043\000\000\000\044\000\037\000\037\000\043\000\037\000\000\000\
\045\000\000\000\046\000\047\000\048\000\045\000\000\000\046\000\
\047\000\048\000\037\000\037\000\037\000\037\000\066\000\037\000\
\037\000\037\000\037\000\000\000\000\000\037\000\037\000\037\000\
\037\000\067\000\068\000\069\000\070\000\067\000\068\000\069\000\
\070\000\089\000\071\000\072\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\000\000\000\000\000\000\
\067\000\068\000\069\000\070\000\118\000\071\000\072\000\073\000\
\074\000\000\000\000\000\075\000\076\000\077\000\078\000\000\000\
\000\000\000\000\000\000\067\000\068\000\069\000\070\000\000\000\
\071\000\072\000\073\000\074\000\000\000\000\000\075\000\076\000\
\077\000\078\000\000\000\050\000\050\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\000\019\000\050\000\050\000\000\000\000\000\000\000\050\000\
\050\000\050\000\050\000\000\000\000\000\050\000\050\000\050\000\
\050\000\038\000\038\000\000\000\038\000\000\000\000\000\067\000\
\068\000\069\000\070\000\000\000\071\000\072\000\073\000\074\000\
\038\000\038\000\075\000\076\000\077\000\038\000\038\000\038\000\
\038\000\000\000\000\000\038\000\038\000\038\000\038\000\039\000\
\039\000\000\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\045\000\000\000\045\000\000\000\039\000\039\000\
\000\000\000\000\000\000\039\000\039\000\039\000\039\000\000\000\
\000\000\039\000\039\000\039\000\039\000\045\000\045\000\045\000\
\045\000\000\000\000\000\045\000\045\000\045\000\045\000\047\000\
\047\000\000\000\047\000\000\000\000\000\000\000\000\000\000\000\
\000\000\046\000\046\000\000\000\046\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\047\000\047\000\047\000\000\000\
\000\000\047\000\047\000\047\000\047\000\046\000\046\000\046\000\
\046\000\000\000\000\000\046\000\046\000\046\000\046\000\044\000\
\044\000\000\000\044\000\042\000\042\000\000\000\042\000\000\000\
\000\000\043\000\043\000\000\000\043\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\000\044\000\044\000\044\000\042\000\
\042\000\044\000\044\000\044\000\044\000\043\000\043\000\042\000\
\042\000\083\000\000\000\000\000\000\000\043\000\043\000\000\000\
\000\000\000\000\000\000\000\000\000\000\067\000\068\000\069\000\
\070\000\107\000\071\000\072\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\067\000\068\000\069\000\
\070\000\109\000\071\000\072\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\000\000\067\000\068\000\069\000\
\070\000\000\000\071\000\072\000\073\000\074\000\000\000\000\000\
\075\000\076\000\077\000\078\000\067\000\068\000\069\000\070\000\
\000\000\071\000\072\000\073\000\074\000\000\000\000\000\075\000\
\076\000\077\000\078\000\067\000\068\000\069\000\070\000\000\000\
\071\000\072\000\073\000\074\000\000\000\000\000\075\000\076\000\
\067\000\068\000\069\000\070\000\000\000\000\000\000\000\073\000\
\074\000\000\000\000\000\075\000\076\000"

let yycheck = "\035\000\
\000\000\000\000\003\001\037\000\018\001\019\001\042\000\043\000\
\044\000\034\001\035\001\036\001\037\001\038\001\002\001\039\001\
\004\001\053\000\001\001\002\001\001\000\004\001\058\000\059\000\
\060\000\040\001\007\001\001\001\064\000\065\000\004\001\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\003\001\045\001\079\000\029\001\030\001\
\003\001\039\001\005\001\004\000\007\001\008\001\009\001\032\001\
\004\001\012\001\013\001\002\001\002\001\004\001\017\001\001\001\
\002\001\045\001\004\001\001\001\002\001\022\000\004\001\039\001\
\108\000\003\001\007\001\111\000\031\001\006\001\033\001\003\001\
\033\000\003\001\118\000\039\001\039\001\001\001\041\001\042\001\
\043\001\003\001\001\001\005\001\030\001\007\001\008\001\009\001\
\004\001\002\001\012\001\013\001\004\001\011\001\003\001\017\001\
\005\001\004\001\007\001\008\001\009\001\004\001\001\001\012\001\
\013\001\001\001\004\001\001\001\017\001\031\001\051\000\033\001\
\004\001\107\000\118\000\109\000\255\255\039\001\255\255\041\001\
\042\001\043\001\031\001\117\000\033\001\255\255\255\255\121\000\
\255\255\255\255\039\001\255\255\041\001\042\001\043\001\003\001\
\255\255\005\001\255\255\007\001\008\001\009\001\255\255\255\255\
\012\001\013\001\255\255\255\255\003\001\017\001\005\001\255\255\
\007\001\003\001\009\001\005\001\255\255\012\001\013\001\255\255\
\255\255\255\255\017\001\031\001\255\255\033\001\255\255\017\001\
\255\255\255\255\255\255\039\001\255\255\041\001\042\001\043\001\
\031\001\255\255\033\001\001\001\002\001\031\001\004\001\255\255\
\039\001\255\255\041\001\042\001\043\001\039\001\255\255\041\001\
\042\001\043\001\016\001\017\001\018\001\019\001\001\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\030\001\016\001\017\001\018\001\019\001\016\001\017\001\018\001\
\019\001\001\001\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\001\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\030\001\255\255\
\255\255\255\255\255\255\016\001\017\001\018\001\019\001\255\255\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\028\001\
\029\001\030\001\255\255\001\001\002\001\255\255\004\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\032\001\016\001\017\001\255\255\255\255\255\255\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\030\001\001\001\002\001\255\255\004\001\255\255\255\255\016\001\
\017\001\018\001\019\001\255\255\021\001\022\001\023\001\024\001\
\016\001\017\001\027\001\028\001\029\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\030\001\001\001\
\002\001\255\255\004\001\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\255\255\016\001\017\001\
\255\255\255\255\255\255\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\030\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\030\001\001\001\
\002\001\255\255\004\001\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\030\001\021\001\022\001\023\001\
\024\001\255\255\255\255\027\001\028\001\029\001\030\001\001\001\
\002\001\255\255\004\001\001\001\002\001\255\255\004\001\255\255\
\255\255\001\001\002\001\255\255\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\021\001\022\001\023\001\024\001\021\001\
\022\001\027\001\028\001\029\001\030\001\021\001\022\001\029\001\
\030\001\004\001\255\255\255\255\255\255\029\001\030\001\255\255\
\255\255\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\004\001\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\255\255\016\001\017\001\018\001\
\019\001\004\001\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\255\255\016\001\017\001\018\001\
\019\001\255\255\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\016\001\017\001\018\001\019\001\
\255\255\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\029\001\030\001\016\001\017\001\018\001\019\001\255\255\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\028\001\
\016\001\017\001\018\001\019\001\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\027\001\028\001"

let yynames_const = "\
  SEMI\000\
  COMMA\000\
  LPR\000\
  RPR\000\
  LBK\000\
  RBK\000\
  LBC\000\
  RBC\000\
  IF\000\
  ELIF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  TRUE\000\
  FALSE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  EQ\000\
  NEQ\000\
  LEQ\000\
  REQ\000\
  RAPPEND\000\
  LAPPEND\000\
  LR\000\
  RL\000\
  AND\000\
  OR\000\
  NOT\000\
  REGEX\000\
  EOF\000\
  RETURN\000\
  BOOL\000\
  VOID\000\
  FLOAT\000\
  STRING\000\
  INT\000\
  "

let yynames_block = "\
  ID\000\
  REGEX_STRING\000\
  STRING_T\000\
  INT_T\000\
  FLOAT_T\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rule_list) in
    Obj.repr(
# 38 "parser.mly"
                             ( {decls = List.rev _1; rules= List.rev _2 } )
# 388 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
                   ( [] )
# 394 "parser.ml"
               : 'fdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 43 "parser.mly"
                       ( _2 :: _1 )
# 402 "parser.ml"
               : 'fdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 47 "parser.mly"
    ( { typ = _1;
    fname = _2;
    formals = _4;
    locals = List.rev _7;
    body = List.rev _8 } )
# 417 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
           ( Void )
# 423 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
             ( String )
# 429 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
            ( Float )
# 435 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
           ( Bool )
# 441 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
          ( Int )
# 447 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                 ( [] )
# 453 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 62 "parser.mly"
                 ( List.rev _1 )
# 460 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
           ( [(_1,_2)] )
# 468 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                               ( (_3,_4) :: _1 )
# 477 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
    ( [] )
# 483 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 70 "parser.mly"
                       ( _2 :: _1 )
# 491 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'assign_opt) in
    Obj.repr(
# 73 "parser.mly"
                           ( (_1, _2, _3) )
# 500 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
    ( Noexpr )
# 506 "parser.ml"
               : 'assign_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                  ( _2 )
# 513 "parser.ml"
               : 'assign_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
    ([])
# 519 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rdecl) in
    Obj.repr(
# 85 "parser.mly"
                      (_2 :: _1)
# 527 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 88 "parser.mly"
                   (_1, _2)
# 535 "parser.ml"
               : 'rdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'single_regex) in
    Obj.repr(
# 91 "parser.mly"
                             (RegexPattern(_2))
# 542 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 94 "parser.mly"
                                 (List.rev _2, List.rev _3)
# 550 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
    ([])
# 556 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                     (_2 :: _1)
# 564 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
            ( Expr _1 )
# 571 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                   ( Return(_2) )
# 578 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 104 "parser.mly"
                    ( Block(List.rev _2) )
# 585 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                    ( If(_3, _5, _7) )
# 594 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                                                    ( For(_3, _5, _7, _9) )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                          ( While(_3, _5) )
# 612 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
     ( Noexpr )
# 618 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
            ( _1 )
# 625 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
                         ( Sliteral(quote_remover(_1)) )
# 632 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 115 "parser.mly"
                         ( Fliteral(_1) )
# 639 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 116 "parser.mly"
                         ( Literal(_1)  )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                       ( Id(_1) )
# 653 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( Binop(_1, Add,   _3) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                       ( Binop(_1, Sub,   _3) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                       ( Binop(_1, Mult,  _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                       ( Binop(_1, Div,   _3) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                       ( Binop(_1, Equal, _3) )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                       ( Binop(_1, Neq,   _3) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( Binop(_1, Less,  _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                       ( Binop(_1, Leq,   _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                       ( Binop(_1, Greater, _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( Binop(_1, Geq,   _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                       ( Binop(_1, And,   _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                       ( Binop(_1, Or,    _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                       ( Unop(Neg, _2) )
# 756 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                       ( Unop(Not, _2) )
# 763 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                       ( Assign(_1, _3) )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
                       ( Call("create", []) )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 134 "parser.mly"
                          ( Call(_1, _3) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                       ( _2 )
# 792 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
    ( [] )
# 798 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 139 "parser.mly"
                ( List.rev _1 )
# 805 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
         ( [_1] )
# 812 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 142 "parser.mly"
                           ( _3 :: _1 )
# 820 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 145 "parser.mly"
                 ( _1 )
# 827 "parser.ml"
               : 'single_regex))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
