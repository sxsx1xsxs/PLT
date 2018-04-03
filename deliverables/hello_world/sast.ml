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

type svar_decl = {
	svtyp  : typ;
	svname : string;
        svexpr : sexpr;
}

type sfunc_decl = {
        sftyp    : typ;
        sfname   : string;
        sformals : svar_decl list;
        slocals  : svar_decl list;
        sbody    : sstmt list;
}

type saction = svar_decl list * sstmt list

type spattern = SRegexPattern of string

type srule = spattern * saction

type sprogram = svar_decl list * sfunc_decl list
