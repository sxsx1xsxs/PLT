(* AST.ml *)
(* This code was original from MicroC. *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type typ = Int | Bool | Float | Void | String | Array_f | Array_s | Array_i
type bind = typ * string

type expr = Literal of int             | BoolLit of bool
          | Fliteral of float          | Id of string
          | Sliteral of string         | Binop of expr * op * expr 
          | Unop of uop * expr         | Assign of string * expr   
          | Call of string * expr list | Noexpr
          | Retrieve of string * expr
          | Array_Assign of string * expr * expr
          | Array_F_Lit of (string * float) list 
          | Array_S_Lit of (string * string) list
          | Array_I_Lit of (string * int) list

type stmt = Block of stmt list | Expr of expr | Return of expr
          | If of expr * stmt * stmt | While of expr * stmt
          | For of expr * expr * expr * stmt

type init = typ * string * expr

(* type decl = var_decl list | func_decl list *)

type var_decl = {
        vtyp  : typ;
        vname : string;
        vexpr : expr;
}

type func_decl = {
        ftyp    : typ;
        fname   : string;
        formals : var_decl list;
        locals  : var_decl list;
        body    : stmt list;
}

type action = var_decl list * stmt list

type pattern = RegexPattern of string

type rule = pattern * action

type program = var_decl list * func_decl list

let string_of_program input_program = "it passes\n"
