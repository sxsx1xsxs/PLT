/* Ocamlyacc parser for CARL
This code was original from MicroC and modified by:
Guanming Qiao; Binwei Xu; Yi Zhang; Chunlin Zhu; Xinjian Wu*/

%{
open Ast
let quote_remover a = String.sub a 1 ((String.length a) - 2);;
%}

%token SEMI COMMA LPR RPR LBK RBK LBC RBC
$token IF ELIF ELSE FOR WHILE
%token TRUE FALSE NULL
%token PLUS MINUS TIMES DIVIDE MOD EQ NEQ LEQ REQ
%token RAPPEND LAPPEND LR RL AND OR NOT
%token REGEX EOF
%token <string> STRING
%token <int> LITERAL

%nonassoc ELSEIF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LR RL LEQ REQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEQ

%start program
%type <Ast.program> program
%%

/*functions are */
program:
decls rule_list EOF { { func_decls =List.rev $1; rules= List.rev $2 } }

/* start of decls */
decls:            { ([], []) }
    | decls vdecl { (($2 :: fst $1), snd $1) }
    | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
    typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { typ = $1;
    fname = $2;
    formals = $4;
    locals = List.rev $7;
    body = List.rev $8 } }

typ:
    | VOID { Void }
    | STRING_T { String_t }
    | FLOAT { Float }
    | BOOL { Bool }
    | INT { Int }

formals_opt:
                 { [] }
 | formal_list   { List.rev $1 }
 
formal_list:
    typ ID { [($1,$2)] }
    | formal_list COMMA typ ID { ($3,$4) :: $1 }

vdecl_list:
                       { [] }
    | vdecl_list vdecl { $2 :: $1 }

vdecl: typ ID assign_opt SEMI { ($1, $2, $3) }

/*Initialize a variable upon declaration */
assign_opt:
                  { Noexpr }
    | ASSIGN expr { $2 }

/* end of decls */

/* start of rule_list */
rule_list:
                      {[]}
    | rule_list rdecl {$2 :: $1}

rdecl:
    pattern action {$1, $2}

pattern:
    REGEX single_regex REGEX {RegexPattern($2)}

action:
    LBC vdecl_list stmt_list RBC {List.rev $2, List.rev $3}
/* end of rule_list */

stmt_list:
    {[]}
    | stmt_list stmt {$2 :: $1}

stmt:
| expr SEMI { Expr $1 }
| RETURN expr SEMI { Return($2) }
| LBC stmt_list RBC { Block(List.rev $2) }
| IF LPR expr RPR stmt %prec NOELSE { If($3, $5, Block([])) }
| IF LPR expr RPR stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPR expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
| WHILE LPR expr RPR stmt { While($3, $5) }

expr_opt:
     { Noexpr }
     | expr { $1 }

expr:
      STRING            { Literal(quote_remover($1)) }
    | FLOAT             { Fliteral($1) }
    | ID               { Id($1) }
    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr LT     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr GT     expr { Binop($1, Greater, $3) }
    | expr GEQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | MINUS expr %prec NEG { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    | ID ASSIGN expr   { Assign($1, $3) }
    | LBK RBK          { Call("create", []) }
    | ID LPR args_opt RPR { Call($1, $3) }
    | LPR expr RPR     { $2 }

args_opt:
    { [] }
    | args_list { List.rev $1 }
args_list:
    expr { [$1] }
    | args_list COMMA expr { $3 :: $1 }
/* start of regex */
regex:
    REGEX_STRING {$1}
/* end of regex */

/* missing stuff: arrays, actuals, noelse, <string> REGEX_STRING as a token*/
