(* AST.ml *)
(* This code was original from MicroC. *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or
type uop = Neg | Not
type typ = Int | Bool | Float | Void | String | Regex
type bind = typ * string

type expr = Literal of int            | BoolLit of bool
          | Fliteral of string        | Id of string
          | Binop of expr * op * expr | Unop of uop * expr
          | Assign of string * expr   | Call of stirng * expr list
          | Noexpr

type stmt = Block of stmt list | Expr of expr | Return of expr
          | If of expr * stmt * stmt | While of expr * stmt
          | For of expr * expr * expr * stmt

type func_decl = {
        typ     : typ;
        fname   : string;
        fornals : bind list;
        locals  : bind list;
        body    : stmt list;
}

type program = bind list * func_decl list
