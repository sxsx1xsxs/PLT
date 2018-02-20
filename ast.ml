(* AST.ml *)
(* This code was original from MicroC. *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type typ = Int | Bool | Float | Void | String
type bind = typ * string

type expr = Literal of int             | BoolLit of bool
          | Fliteral of float          | Id of string
          | Sliteral of string         | Binop of expr * op * expr 
          | Unop of uop * expr         | Assign of string * expr   
          | Call of string * expr list | Noexpr

type stmt = Block of stmt list | Expr of expr | Return of expr
          | If of expr * stmt * stmt | While of expr * stmt
          | For of expr * expr * expr * stmt

type init = typ * string * expr

type func_decl = {
        typ     : typ;
        fname   : string;
        formals : bind list;
        locals  : init list;
        body    : stmt list;
}

type action = init list * stmt list

type pattern = RegexPattern of string

type rule = pattern * action

type program = {
    decls: func_decl list;
    rules: rule list;
}

let string_of_program input_program = "\n it passes"
