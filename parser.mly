/* Ocamlyacc parser for OpenFile
This code was original from MicroC and modified by:
Guanming Qiao; Binwei Xu; Yi Zhang; Chunlin Zhu; Xinjian Wu */

%{
open Ast
let quote_remover a = String.sub a 1 ((String.length a) - 2);;
%}

%token SEMI COMMA LPR RPR LBK RBK LBC RBC
%token IF ELIF ELSE FOR WHILE
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MOD EQ NEQ LEQ REQ
%token RAPPEND LAPPEND LR RL AND OR NOT
%token REGEX EOF ASSIGN 
%token RETURN BOOL VOID FLOAT STRING INT
%token <string> ID REGEX_STRING STRING_T
%token <int> INT_T
%token <float> FLOAT_T

%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LR RL LEQ REQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%start program
%type <Ast.program> program
%%

/* functions are */
program:
    { [], [] }
    | program vdecl    { ($2 :: fst $1), snd $1 }
    | program fdecl    { fst $1, ($2 :: snd $1) }

/* start of decls */
fdecl:
    typ ID LPR formal_list RPR LBC vdecl_list stmt_list RBC
    { { ftyp = $1;
        fname = $2;
        formals = $4;
        locals = List.rev $7;
        body = List.rev $8 } }

typ:
    | VOID { Void }
    | STRING { String }
    | FLOAT { Float }
    | BOOL { Bool }
    | INT { Int }

formal_list:
                    { [] }
    | formal_list formal_decl  { $2 :: $1 }

formal_decl:
    typ ID         { VarDecl($1, $2, Noexpr) }
    | COMMA typ ID { VarDecl($2, $3, Noexpr) }

vdecl_list:
                       { [] }
    | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { VarDecl($1, $2, Noexpr) }
    | typ ID ASSIGN expr SEMI { VarDecl($1, $2, $4) }

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
| IF LPR expr RPR stmt ELSE stmt    { If($3, $5, $7) }
| FOR LPR expr_opt SEMI expr SEMI expr_opt RPR stmt { For($3, $5, $7, $9) }
| WHILE LPR expr RPR stmt { While($3, $5) }

expr_opt:
     { Noexpr }
     | expr { $1 }

expr:
      STRING_T           { Sliteral(quote_remover($1)) }
    | FLOAT_T            { Fliteral($1) }
    | INT_T              { Literal($1)  }
    | ID               { Id($1) }
	| LBK expr_list RBK { Array_Lit($2) }
    | expr PLUS   expr { Binop($1, Add,   $3) }
    | expr MINUS  expr { Binop($1, Sub,   $3) }
    | expr TIMES  expr { Binop($1, Mult,  $3) }
    | expr DIVIDE expr { Binop($1, Div,   $3) }
    | expr EQ     expr { Binop($1, Equal, $3) }
    | expr NEQ    expr { Binop($1, Neq,   $3) }
    | expr RL     expr { Binop($1, Less,  $3) }
    | expr LEQ    expr { Binop($1, Leq,   $3) }
    | expr LR     expr { Binop($1, Greater, $3) }
    | expr REQ    expr { Binop($1, Geq,   $3) }
    | expr AND    expr { Binop($1, And,   $3) }
    | expr OR     expr { Binop($1, Or,    $3) }
    | MINUS       expr { Unop(Neg, $2) }
    | NOT expr         { Unop(Not, $2) }
    | expr ASSIGN expr   { Assign($1, $3) }
    | LBK RBK          { Call("create", []) }
    | expr LBK expr RBK { Array_Index($1, $3) }
    | ID LPR args_opt RPR { Call($1, $3) }
    | LPR expr RPR     { $2 }

expr_list:
    | expr { [$1] }
    | expr COMMA expr_list { $1 :: $3 }

array_list:
	/* nothing */ { [] }
	| LBK INT_T RBK array_list { $2 :: $4 }

bind:
	| typ ID array_list
	{ List.fold_right (fun l (name, typ) -> (name, Arr(typ, l))) $3 ($2, $1) }

args_opt:
    { [] }
    | args_list { List.rev $1 }
args_list:
    expr { [$1] }
    | args_list COMMA expr { $3 :: $1 }

/* start of regex */
single_regex:
    REGEX_STRING { $1 }
/* end of regex */

/* missing stuff: arrays, actuals, noelse, <string> REGEX_STRING as a token*/
