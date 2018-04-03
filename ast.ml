(* AST.ml *)
(* This code was original from MicroC. *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Bool | Float | Void | String (* | Array_f | Array_s | Array_i *)

type expr = Literal of int             | BoolLit of bool
          | Fliteral of float          | Id of string
          | Sliteral of string         | Binop of expr * op * expr 
          | Unop of uop * expr         | Assign of string * expr   
          | Call of string * expr list | Noexpr
          | Retrieve of string * expr
          | Array_Assign of string * expr * expr
(*           | Array_F_Lit of (string * float) list 
          | Array_S_Lit of (string * string) list
          | Array_I_Lit of (string * int) list *)


type stmt = Block of stmt list 
          | Expr of expr 
          | Return of expr
          | If of expr * stmt * stmt 
          | While of expr * stmt
          | For of expr * expr * expr * stmt


(* type decl = var_decl list | func_decl list *)

type var_decl = typ * string * expr

type func_decl = {
        ftyp    : typ;
        fname   : string;
        formals : var_decl list;
        locals  : var_decl list;
        body    : stmt list;
}

type pattern = RegexPattern of string

type program = var_decl list * func_decl list

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> string_of_float l
  | Sliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Retrieve (n, e) -> n ^ "[" ^ string_of_expr e ^ "]"
  | Array_Assign (n, e1, e2) -> n ^ "[" ^ string_of_expr e1 ^ "]" ^ " = " ^ string_of_expr e2

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl = function
  |(t, id, Noexpr) = string_of_typ t ^ " " ^ id
  |(t, id, e) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^
  ")\n{\n" ^
  String.concat ";\n" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_pattern x = "@" ^ x ^ "@"

let string_of_program (vars, funcs) =
  String.concat ";\n" (List.map string_of_vdecl vars) ^ ";\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

