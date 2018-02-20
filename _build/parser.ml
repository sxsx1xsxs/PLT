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
\001\000\002\000\002\000\002\000\005\000\006\000\006\000\006\000\
\006\000\006\000\007\000\007\000\010\000\010\000\008\000\008\000\
\004\000\011\000\011\000\003\000\003\000\013\000\014\000\015\000\
\009\000\009\000\017\000\017\000\017\000\017\000\017\000\017\000\
\018\000\018\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\019\000\019\000\020\000\020\000\016\000\000\000"

let yylen = "\002\000\
\003\000\000\000\002\000\002\000\009\000\001\000\001\000\001\000\
\001\000\001\000\000\000\001\000\002\000\004\000\000\000\002\000\
\004\000\000\000\002\000\000\000\002\000\002\000\003\000\004\000\
\000\000\002\000\002\000\003\000\003\000\007\000\009\000\005\000\
\000\000\001\000\001\000\001\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\002\000\004\000\003\000\
\000\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\062\000\000\000\009\000\006\000\008\000\007\000\
\010\000\000\000\003\000\004\000\000\000\000\000\001\000\021\000\
\000\000\000\000\061\000\000\000\015\000\022\000\000\000\000\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\037\000\036\000\000\000\017\000\
\016\000\000\000\000\000\013\000\000\000\000\000\000\000\054\000\
\000\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\024\000\000\000\000\000\000\000\000\000\000\000\
\026\000\015\000\000\000\056\000\000\000\000\000\000\000\000\000\
\000\000\000\000\041\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\000\000\014\000\055\000\000\000\029\000\000\000\
\000\000\000\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\032\000\000\000\000\000\030\000\
\000\000\000\000\031\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\041\000\012\000\042\000\029\000\027\000\
\043\000\030\000\025\000\072\000\016\000\017\000\022\000\020\000\
\073\000\106\000\078\000\079\000"

let yysindex = "\004\000\
\000\000\000\000\000\000\039\255\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\235\254\241\254\000\000\000\000\
\037\255\001\255\000\000\243\254\000\000\000\000\039\255\161\255\
\049\255\000\000\039\255\041\255\054\255\082\255\161\255\080\255\
\161\255\161\255\003\255\000\000\000\000\000\000\238\000\000\000\
\000\000\061\255\048\255\000\000\086\255\039\255\191\000\000\000\
\249\254\000\000\161\255\161\255\161\255\161\255\161\255\161\255\
\161\255\161\255\161\255\161\255\161\255\161\255\161\255\161\255\
\058\255\000\000\000\000\096\255\101\255\105\255\161\255\208\255\
\000\000\000\000\073\255\000\000\238\000\109\255\114\255\238\000\
\249\254\249\254\000\000\000\000\010\001\010\001\050\255\050\255\
\050\255\050\255\253\000\041\000\089\255\161\255\161\255\161\255\
\227\255\000\000\039\255\000\000\000\000\161\255\000\000\207\000\
\238\000\116\255\223\000\000\000\102\255\238\000\156\255\161\255\
\156\255\000\000\107\255\246\255\000\000\156\255\161\255\000\000\
\117\255\156\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\122\255\000\000\000\000\000\000\000\000\120\255\000\000\
\000\000\000\000\143\255\000\000\000\000\121\255\000\000\000\000\
\000\000\000\000\189\255\000\000\000\000\000\000\125\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\000\000\123\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\122\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\022\255\000\000\130\255\081\255\
\050\000\080\000\000\000\000\000\164\000\170\000\090\000\120\000\
\130\000\160\000\013\255\019\255\000\000\000\000\128\255\000\000\
\000\000\000\000\143\255\000\000\000\000\000\000\000\000\000\000\
\012\255\000\000\000\000\000\000\000\000\060\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\132\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\133\000\000\000\255\255\000\000\064\000\
\198\255\000\000\000\000\232\255\000\000\000\000\000\000\000\000\
\197\255\021\000\000\000\000\000"

let yytablesize = 550
let yytable = "\039\000\
\015\000\020\000\013\000\023\000\001\000\051\000\047\000\093\000\
\049\000\050\000\055\000\056\000\034\000\049\000\049\000\034\000\
\049\000\018\000\026\000\050\000\050\000\028\000\050\000\059\000\
\019\000\059\000\077\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\109\000\049\000\049\000\021\000\075\000\024\000\097\000\052\000\
\050\000\040\000\031\000\115\000\032\000\117\000\066\000\067\000\
\068\000\045\000\120\000\069\000\070\000\060\000\123\000\060\000\
\033\000\053\000\054\000\055\000\056\000\104\000\105\000\107\000\
\005\000\006\000\007\000\008\000\009\000\110\000\034\000\044\000\
\071\000\053\000\053\000\046\000\053\000\048\000\035\000\116\000\
\036\000\037\000\038\000\031\000\074\000\032\000\105\000\066\000\
\103\000\068\000\094\000\065\000\069\000\070\000\024\000\095\000\
\031\000\033\000\032\000\096\000\066\000\114\000\068\000\100\000\
\101\000\069\000\070\000\102\000\112\000\118\000\033\000\034\000\
\122\000\071\000\018\000\011\000\012\000\019\000\057\000\035\000\
\033\000\036\000\037\000\038\000\034\000\058\000\071\000\033\000\
\011\000\099\000\000\000\121\000\035\000\000\000\036\000\037\000\
\038\000\025\000\000\000\025\000\000\000\025\000\025\000\025\000\
\000\000\000\000\025\000\025\000\000\000\000\000\031\000\025\000\
\032\000\000\000\066\000\031\000\068\000\032\000\000\000\069\000\
\070\000\000\000\000\000\000\000\033\000\025\000\000\000\025\000\
\000\000\033\000\000\000\000\000\000\000\025\000\000\000\025\000\
\025\000\025\000\034\000\000\000\071\000\038\000\038\000\034\000\
\038\000\000\000\035\000\000\000\036\000\037\000\038\000\035\000\
\000\000\036\000\037\000\038\000\038\000\038\000\038\000\038\000\
\098\000\038\000\038\000\038\000\038\000\000\000\000\000\038\000\
\038\000\038\000\038\000\000\000\000\000\000\000\000\000\053\000\
\054\000\055\000\056\000\108\000\057\000\058\000\059\000\060\000\
\000\000\000\000\061\000\062\000\063\000\064\000\000\000\000\000\
\000\000\000\000\053\000\054\000\055\000\056\000\119\000\057\000\
\058\000\059\000\060\000\000\000\000\000\061\000\062\000\063\000\
\064\000\000\000\000\000\000\000\000\000\053\000\054\000\055\000\
\056\000\000\000\057\000\058\000\059\000\060\000\000\000\000\000\
\061\000\062\000\063\000\064\000\051\000\051\000\000\000\051\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\020\000\000\000\051\000\051\000\000\000\000\000\000\000\
\051\000\051\000\051\000\051\000\000\000\000\000\051\000\051\000\
\051\000\051\000\039\000\039\000\000\000\039\000\000\000\000\000\
\053\000\054\000\055\000\056\000\000\000\057\000\058\000\059\000\
\060\000\039\000\039\000\061\000\062\000\063\000\039\000\039\000\
\039\000\039\000\000\000\000\000\039\000\039\000\039\000\039\000\
\040\000\040\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\046\000\000\000\046\000\000\000\040\000\
\040\000\000\000\000\000\000\000\040\000\040\000\040\000\040\000\
\000\000\000\000\040\000\040\000\040\000\040\000\046\000\046\000\
\046\000\046\000\000\000\000\000\046\000\046\000\046\000\046\000\
\048\000\048\000\000\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\047\000\000\000\047\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\000\048\000\048\000\048\000\
\000\000\000\000\048\000\048\000\048\000\048\000\047\000\047\000\
\047\000\047\000\000\000\000\000\047\000\047\000\047\000\047\000\
\045\000\045\000\000\000\045\000\043\000\043\000\000\000\043\000\
\000\000\000\000\044\000\044\000\000\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\045\000\045\000\045\000\
\043\000\043\000\045\000\045\000\045\000\045\000\044\000\044\000\
\043\000\043\000\076\000\000\000\000\000\000\000\044\000\044\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\054\000\
\055\000\056\000\111\000\057\000\058\000\059\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\000\000\053\000\054\000\
\055\000\056\000\113\000\057\000\058\000\059\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\000\000\053\000\054\000\
\055\000\056\000\000\000\057\000\058\000\059\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\053\000\054\000\055\000\
\056\000\000\000\057\000\058\000\059\000\060\000\000\000\000\000\
\061\000\062\000\063\000\064\000\053\000\054\000\055\000\056\000\
\000\000\057\000\058\000\059\000\060\000\000\000\000\000\061\000\
\062\000\053\000\054\000\055\000\056\000\000\000\000\000\000\000\
\059\000\060\000\000\000\000\000\061\000\062\000"

let yycheck = "\024\000\
\000\000\000\000\004\000\003\001\001\000\003\001\031\000\066\000\
\033\000\034\000\018\001\019\001\001\001\001\001\002\001\004\001\
\004\001\039\001\032\001\001\001\002\001\023\000\004\001\002\001\
\040\001\004\001\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\099\000\029\001\030\001\007\001\046\000\045\001\071\000\045\001\
\030\001\001\001\003\001\111\000\005\001\113\000\007\001\008\001\
\009\001\004\001\118\000\012\001\013\001\002\001\122\000\004\001\
\017\001\016\001\017\001\018\001\019\001\094\000\095\000\096\000\
\034\001\035\001\036\001\037\001\038\001\102\000\031\001\039\001\
\033\001\001\001\002\001\002\001\004\001\006\001\039\001\112\000\
\041\001\042\001\043\001\003\001\007\001\005\001\119\000\007\001\
\008\001\009\001\003\001\039\001\012\001\013\001\045\001\003\001\
\003\001\017\001\005\001\003\001\007\001\008\001\009\001\039\001\
\004\001\012\001\013\001\002\001\001\001\011\001\017\001\031\001\
\004\001\033\001\001\001\004\001\004\001\001\001\004\001\039\001\
\001\001\041\001\042\001\043\001\031\001\004\001\033\001\004\001\
\004\000\074\000\255\255\119\000\039\001\255\255\041\001\042\001\
\043\001\003\001\255\255\005\001\255\255\007\001\008\001\009\001\
\255\255\255\255\012\001\013\001\255\255\255\255\003\001\017\001\
\005\001\255\255\007\001\003\001\009\001\005\001\255\255\012\001\
\013\001\255\255\255\255\255\255\017\001\031\001\255\255\033\001\
\255\255\017\001\255\255\255\255\255\255\039\001\255\255\041\001\
\042\001\043\001\031\001\255\255\033\001\001\001\002\001\031\001\
\004\001\255\255\039\001\255\255\041\001\042\001\043\001\039\001\
\255\255\041\001\042\001\043\001\016\001\017\001\018\001\019\001\
\001\001\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\029\001\030\001\255\255\255\255\255\255\255\255\016\001\
\017\001\018\001\019\001\001\001\021\001\022\001\023\001\024\001\
\255\255\255\255\027\001\028\001\029\001\030\001\255\255\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\001\001\021\001\
\022\001\023\001\024\001\255\255\255\255\027\001\028\001\029\001\
\030\001\255\255\255\255\255\255\255\255\016\001\017\001\018\001\
\019\001\255\255\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\001\001\002\001\255\255\004\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001\032\001\255\255\016\001\017\001\255\255\255\255\255\255\
\021\001\022\001\023\001\024\001\255\255\255\255\027\001\028\001\
\029\001\030\001\001\001\002\001\255\255\004\001\255\255\255\255\
\016\001\017\001\018\001\019\001\255\255\021\001\022\001\023\001\
\024\001\016\001\017\001\027\001\028\001\029\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\028\001\029\001\030\001\
\001\001\002\001\255\255\004\001\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\002\001\255\255\004\001\255\255\016\001\
\017\001\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\255\255\255\255\027\001\028\001\029\001\030\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\028\001\029\001\030\001\
\001\001\002\001\255\255\004\001\255\255\255\255\255\255\255\255\
\255\255\255\255\001\001\002\001\255\255\004\001\255\255\255\255\
\255\255\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\255\255\255\255\027\001\028\001\029\001\030\001\021\001\022\001\
\023\001\024\001\255\255\255\255\027\001\028\001\029\001\030\001\
\001\001\002\001\255\255\004\001\001\001\002\001\255\255\004\001\
\255\255\255\255\001\001\002\001\255\255\004\001\255\255\255\255\
\255\255\255\255\255\255\255\255\021\001\022\001\023\001\024\001\
\021\001\022\001\027\001\028\001\029\001\030\001\021\001\022\001\
\029\001\030\001\004\001\255\255\255\255\255\255\029\001\030\001\
\255\255\255\255\255\255\255\255\255\255\255\255\016\001\017\001\
\018\001\019\001\004\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\030\001\255\255\016\001\017\001\
\018\001\019\001\004\001\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\030\001\255\255\016\001\017\001\
\018\001\019\001\255\255\021\001\022\001\023\001\024\001\255\255\
\255\255\027\001\028\001\029\001\030\001\016\001\017\001\018\001\
\019\001\255\255\021\001\022\001\023\001\024\001\255\255\255\255\
\027\001\028\001\029\001\030\001\016\001\017\001\018\001\019\001\
\255\255\021\001\022\001\023\001\024\001\255\255\255\255\027\001\
\028\001\016\001\017\001\018\001\019\001\255\255\255\255\255\255\
\023\001\024\001\255\255\255\255\027\001\028\001"

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
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rule_list) in
    Obj.repr(
# 38 "parser.mly"
                    ( { func_decls =List.rev _1; rules= List.rev _2 } )
# 388 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                  ( ([], []) )
# 394 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 42 "parser.mly"
                  ( ((_2 :: fst _1), snd _1) )
# 402 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 43 "parser.mly"
                  ( (fst _1, (_2 :: snd _1)) )
# 410 "parser.ml"
               : 'decls))
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
# 425 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
           ( Void )
# 431 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
             ( String )
# 437 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
            ( Float )
# 443 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
           ( Bool )
# 449 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
          ( Int )
# 455 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                 ( [] )
# 461 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 62 "parser.mly"
                 ( List.rev _1 )
# 468 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
           ( [(_1,_2)] )
# 476 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                               ( (_3,_4) :: _1 )
# 485 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                       ( [] )
# 491 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 70 "parser.mly"
                       ( _2 :: _1 )
# 499 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'assign_opt) in
    Obj.repr(
# 72 "parser.mly"
                              ( (_1, _2, _3) )
# 508 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                  ( Noexpr )
# 514 "parser.ml"
               : 'assign_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                  ( _2 )
# 521 "parser.ml"
               : 'assign_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                      ([])
# 527 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rdecl) in
    Obj.repr(
# 84 "parser.mly"
                      (_2 :: _1)
# 535 "parser.ml"
               : 'rule_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pattern) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 87 "parser.mly"
                   (_1, _2)
# 543 "parser.ml"
               : 'rdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'single_regex) in
    Obj.repr(
# 90 "parser.mly"
                             (RegexPattern(_2))
# 550 "parser.ml"
               : 'pattern))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 93 "parser.mly"
                                 (List.rev _2, List.rev _3)
# 558 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
    ([])
# 564 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 98 "parser.mly"
                     (_2 :: _1)
# 572 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
            ( Expr _1 )
# 579 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                   ( Return(_2) )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 103 "parser.mly"
                    ( Block(List.rev _2) )
# 593 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                    ( If(_3, _5, _7) )
# 602 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 105 "parser.mly"
                                                    ( For(_3, _5, _7, _9) )
# 612 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 106 "parser.mly"
                          ( While(_3, _5) )
# 620 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
     ( Noexpr )
# 626 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
            ( _1 )
# 633 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
                         ( Sliteral(quote_remover(_1)) )
# 640 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 114 "parser.mly"
                         ( Fliteral(_1) )
# 647 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "parser.mly"
                         ( Literal(_1)  )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                       ( Id(_1) )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                       ( Binop(_1, Add,   _3) )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( Binop(_1, Sub,   _3) )
# 677 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                       ( Binop(_1, Mult,  _3) )
# 685 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                       ( Binop(_1, Div,   _3) )
# 693 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                       ( Binop(_1, Equal, _3) )
# 701 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                       ( Binop(_1, Neq,   _3) )
# 709 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                       ( Binop(_1, Less,  _3) )
# 717 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                       ( Binop(_1, Leq,   _3) )
# 725 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                       ( Binop(_1, Greater, _3) )
# 733 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                       ( Binop(_1, Geq,   _3) )
# 741 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                       ( Binop(_1, And,   _3) )
# 749 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                       ( Binop(_1, Or,    _3) )
# 757 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                       ( Unop(Neg, _2) )
# 764 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                       ( Unop(Not, _2) )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 131 "parser.mly"
                       ( Assign(_1, _3) )
# 779 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
                       ( Call("create", []) )
# 785 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 133 "parser.mly"
                          ( Call(_1, _3) )
# 793 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                       ( _2 )
# 800 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
    ( [] )
# 806 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 138 "parser.mly"
                ( List.rev _1 )
# 813 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 140 "parser.mly"
         ( [_1] )
# 820 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 141 "parser.mly"
                           ( _3 :: _1 )
# 828 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 144 "parser.mly"
                 ( _1 )
# 835 "parser.ml"
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
