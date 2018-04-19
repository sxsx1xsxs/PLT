(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx = 
	SLiteral of int               | SBoolLit of bool
	| SFliteral of float          | SId of string
	| SSliteral of string         | SBinop of sexpr * op * sexpr 
	| SUnop of uop * sexpr         | SAssign of string * sexpr   
	| SCall of string * sexpr list | SNoexpr
	| SRetrieve of string * sexpr
	| SArray_Assign of string * sexpr * sexpr
	| SArray_F_Lit of (string * float) list 
	| SArray_S_Lit of (string * string) list
	| SArray_I_Lit of (string * int) list
	
type sstmt = 
	SBlock of sstmt list | SExpr of sexpr | SReturn of sexpr
          | SIf of sexpr * sstmt * sstmt | SWhile of sexpr * sstmt
          | SFor of sexpr * sexpr * sexpr * sstmt
		  
type sinit = typ * string * sexpr

type svar_decl = SVarDecl of typ * string * sexpr

type sfunc_decl = {
        sftyp    : typ;
        sfname   : string;
        sformals : var_decl list;
        slocals  : var_decl list;
        sbody    : sstmt list;
}


type sprogram = svar_decl list * sfunc_decl list
