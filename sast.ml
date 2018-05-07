(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx = 
	SLiteral of int               | SBoolLit of bool
	| SFliteral of float          | SId of string
	| SSliteral of string         | SBinop of sexpr * op * sexpr 
	| SUnop of uop * sexpr         | SAssign of sexpr * sexpr   
	| SCall of string * sexpr list | SNoexpr
  | SArray_Index of expr * expr
  | SArray_Lit of expr list

	
type sstmt = 
	SBlock of sstmt list | SExpr of sexpr | SReturn of sexpr
          | SIf of sexpr * sstmt * sstmt | SWhile of sexpr * sstmt
          | SFor of sexpr * sexpr * sexpr * sstmt
		  
type sinit = typ * string * sexpr

type svar_decl = typ * string * sexpr

type sfunc_decl = {
        sftyp    : typ;
        sfname   : string;
        sformals : var_decl list;
        slocals  : var_decl list;
        sbody    : sstmt list;
}


type sprogram = svar_decl list * sfunc_decl list
