(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Ast 

module StringMap = Map.Make(String)


let translate (globals, functions) =
    (* Code Generation from the SAST. Returns an LLVM module if successful,
     throws an exception if something is wrong. *)
  let context    = L.global_context () in
  let the_module = L.create_module context "OpenFile" in
  (* Add types to the context so we can use them in our LLVM code *)
    
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  let void_p     = L.pointer_type i8_t
  and str_t      = L.pointer_type i8_t in
  (* and arr_t      = L.i64_type context in *)

  (* Convert OpenFile types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> str_t
    | A.Float -> float_t
    | A.Void  -> void_t
    (* | A.Array_f -> void_p
    | A.Array_s -> void_p
    | A.Array_i -> void_p *)
  in

  let global_init_expr = function
      A.Literal i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Sliteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in 
    L.const_bitcast (L.const_gep l [| L.const_int i32_t 0|]) str_t
    | A.Fliteral f -> L.const_float float_t f
    | A.Noexpr -> L.const_int i32_t 0
    | _ -> raise (Failure ("not found"))
  in

  let global_init_noexpr = function
    | A.Int -> L.const_int i32_t 0
    | A.Bool -> L.const_int i1_t 0
    | A.String -> global_init_expr(A.Sliteral "")
    | A.Void -> L.const_int void_t 0
    | A.Float -> L.const_float float_t 0.0
    (* | A.Array_f -> L.const_pointer_null void_p
    | A.Array_s -> L.const_pointer_null void_p
    | A.Array_i -> L.const_pointer_null void_p *)
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars = 
    let global_var m (t, n, e) =
      let init = global_init_expr e in
      StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in
  
  let strlen_t = L.function_type float_t [| str_t |] in
  let strlen_func = L.declare_function "strlen" strlen_t the_module in
  
  (* let create_t = L.function_type void_p [||] in
  let create_func = L.declare_function "create" create_t the_module in
  *)
  let array_add_string_t = L.function_type i32_t [|void_p ; str_t ; str_t|] in
  let array_add_string_func = L.declare_function "array_add_string" array_add_string_t the_module in

  let array_retrieve_string_t = L.function_type str_t [|void_p ; str_t|] in
  let array_retrieve_string_func = L.declare_function "array_retrieve_string" array_retrieve_string_t the_module in

  let array_add_float_t = L.function_type i32_t [|void_p ; str_t ; float_t|] in
  let array_add_float_func = L.declare_function "array_add_float" array_add_float_t the_module in

  let array_retrieve_float_t = L.function_type float_t [|void_p ; str_t|] in
  let array_retrieve_float_func = L.declare_function "array_retrieve_float" array_retrieve_float_t the_module in
  
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.fname
      and formal_types = 
	Array.of_list (List.map (fun (t,_,_) -> ltype_of_typ t ) fdecl.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.fname function_decls 
  with Not_found -> raise (Failure "115")
  in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (*let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder*)
    let char_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    (* and string_format_str = L.build_global_stringptr "%s\n" "fmt2" builder *) 
    and float_format_str = L.build_global_stringptr "f\n" "fmt" builder
    in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_init_expr = function
        A.Literal i -> L.const_int i32_t i
        | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | A.Sliteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in L.const_bitcast (L.const_gep l [| L.const_int i32_t 0|]) str_t
        | A.Fliteral f -> L.const_float float_t f
        | A.Noexpr -> L.const_int i32_t 0
        | _ -> raise (Failure ("not found"))
    in

    let local_init_noexpr = function
        | A.Int -> L.const_int i32_t 0
        | A.Bool -> L.const_int i1_t 0
        | A.String -> global_init_expr(A.Sliteral "")
        | A.Void -> L.const_int void_t 0
        | A.Float -> L.const_float float_t 0.0
        (* | A.Array_f -> L.const_pointer_null void_p
        | A.Array_s -> L.const_pointer_null void_p
        | A.Array_i -> L.const_pointer_null void_p *)
    in

    let local_vars =
      let add_formal m (t, n, _) p = 
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	    StringMap.add n local m 
        in

    (* build_array_access 4/2 Xingjian *)
    (* let build_array_access g_map l_map s i1 i2 builder isAssign 
      if isAssign
        then L.build_gep(lookup g_map l_map s) [| i1; i2 |] s builder
        else L.builder_load (L.build_gep(lookup g_map l_map s) [| i1; i2 |] s builder) s builder
    in*)

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n, e) =
        let e' = match e with
            A.Noexpr -> global_init_noexpr t
            | _ -> global_init_expr e
        in
    L.set_value_name n e';
	let l_var = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store e' l_var builder);
    StringMap.add n l_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.locals in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> 
                        try StringMap.find n global_vars with
                          Not_found -> raise (Failure "150")
                      in
    (* let lookup_func n = fst (StringMap.find n function_decls) in *)

    let build_string e builder = 
        let str = L.build_global_stringptr e "str" builder in
        let null = L.const_int i32_t 0 in
    L.build_in_bounds_gep str [| null |] "str" builder in

    (* This is where GM left at Fri. 8:05 PM *)

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        Literal i -> L.const_int i32_t i
      | BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | Fliteral l -> L.const_float float_t l
      | Sliteral s -> build_string s builder
      | Noexpr -> L.const_int i32_t 0
      | Id s -> L.build_load (lookup s) s builder
      | Binop (e1, op, e2) ->
	  (* let (t, _) = e1 *)
	  let e1' = expr builder e1
	  and e2' = expr builder e2 in
	  ( match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mult    -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And     -> L.build_and
          | A.Or      -> L.build_or
	  ) e1' e2' "tmp" builder
      | Unop(op, e) ->
	  let e' = expr builder e in
	  (match op with
	  | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      
    
    (* ArrayLit, ArrayAssign, ArrayIndex, ArrayAccess 4/2 Xingjian*)
    (* | A.Array_Index(s, ind1) ->
      let i = expr builder g_map l_map ind1 in
        build_array_access g_map l_map s(L.const_int i32_t 0) i builder false                           
    | A.Array_F_Lit(l, f) ->
		let size = L.const_int i32_t (List.length l) in
		let all = List.map (fun e -> expr builder g_map l_map e) l in
		let new_array = L.build_array_malloc (L.type_of (List.hd all)) size "tmp" builder in
		List.iter (fun x ->
			let more = (L.build_gep new_array [| L.const_int i32_t x |] "tmp2" builder) in
			let intermediate = List.nth all x in
			ignore (L.build_store intermediate more builder)
		) (int_range (List.length l)) ;
		let type_of_new_literal = L.struct_type context [| i32_t ; L.pointer_type (L.type_of (List.hd all)) |] in
		let new_literal = L.build_malloc type_of_new_literal "arr_literal" builder in
		let first_store = L.build_struct_gep new_literal 0 "first" builder in
		let second_store = L.build_struct_gep new_literal 1 "second" builder in
		ignore (L.build_store size first_store builder);
		ignore (L.build_store new_array second_store builder);
		let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
		actual_literal
	| A.Array_S_Lit(l, s) ->
		let size = L.const_int i32_t (List.length l) in
		let all = List.map (fun e -> expr builder g_map l_map e) l in
		let new_array = L.build_array_malloc (L.type_of (List.hd all)) size "tmp" builder in
		List.iter (fun x ->
			let more = (L.build_gep new_array [| L.const_int i32_t x |] "tmp2" builder) in
			let intermediate = List.nth all x in
			ignore (L.build_store intermediate more builder)
		) (int_range (List.length l)) ;
		let type_of_new_literal = L.struct_type context [| i32_t ; L.pointer_type (L.type_of (List.hd all)) |] in
		let new_literal = L.build_malloc type_of_new_literal "arr_literal" builder in
		let first_store = L.build_struct_gep new_literal 0 "first" builder in
		let second_store = L.build_struct_gep new_literal 1 "second" builder in
		ignore (L.build_store size first_store builder);
		ignore (L.build_store new_array second_store builder);
		let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
		actual_literal
	| A.Array_I_Lit(l, i) ->
		let size = L.const_int i32_t (List.length l) in
		let all = List.map (fun e -> expr builder g_map l_map e) l in
		let new_array = L.build_array_malloc (L.type_of (List.hd all)) size "tmp" builder in
		List.iter (fun x ->
			let more = (L.build_gep new_array [| L.const_int i32_t x |] "tmp2" builder) in
			let intermediate = List.nth all x in
			ignore (L.build_store intermediate more builder)
		) (int_range (List.length l)) ;
		let type_of_new_literal = L.struct_type context [| i32_t ; L.pointer_type (L.type_of (List.hd all)) |] in
		let new_literal = L.build_malloc type_of_new_literal "arr_literal" builder in
		let first_store = L.build_struct_gep new_literal 0 "first" builder in
		let second_store = L.build_struct_gep new_literal 1 "second" builder in
		ignore (L.build_store size first_store builder);
		ignore (L.build_store new_array second_store builder);
		let actual_literal = L.build_load new_literal "actual_arr_literal" builder in
		actual_literal
	| A.Array_Assign(v, i, e) -> let e' = expr builder g_map l_map e in 
                                  let i' = expr builder g_map l_map (List.hd i) in
                                  let v' = L.build_load (lookup g_map l_map v) v builder in 
                                  let extract_array = L.build_extractvalue v' 1 "extract_ptr" builder in
                                  let extract_value = L.build_gep extract_array [| i' |] "extract_value" builder in
                                  ignore (L.build_store e' extract_value builder); e'
    *)

      (* assume only float need semantic checking *)
      | Array_Index (s, e) -> L.build_call array_retrieve_float_func [|(L.build_load (lookup s) s builder) ; (expr builder e)|] "array_retrieve" builder
      | Array_Assign (s, i, e) -> L.build_call array_add_float_func [|(L.build_load (lookup s) s builder) ; (expr builder i) ; (expr builder e)|]
            "array_add_float" builder
      | Assign (s, e) -> let e' = expr builder e in
                         let _  = L.build_store e' (lookup s) builder in e'
      (*| Call ("prints", [_]) -> L.build_call prints_func [| str_format_str |] "prints" builder*)
      | Call ("prints", [e]) -> L.build_call printf_func [| char_format_str; (expr builder e)|] "printf" builder
      | Call ("print", [e]) | Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
      | Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | Call ("printf", [e]) -> 
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | Call ("strlen", [e]) ->
    L.build_call strlen_func [|expr builder e|] 
    "strlen" builder
      | Call ("array_add_string", [a;b;c]) ->
    L.build_call array_add_string_func [|(expr builder a) ; (expr builder b) ; (expr builder c)|] 
    "array_add_string" builder
      | Call ("array_retrieve_string", [a; b]) ->
    L.build_call array_retrieve_string_func [|(expr builder a) ; (expr builder b)|]
    "array_retrieve_string" builder
      | Call ("array_add_float", [a; b; c]) ->
    L.build_call array_add_float_func [|(expr builder a) ; (expr builder b) ; (expr builder c)|]
    "array_add_float" builder
      | Call ("array_retrieve_float", [a; b]) ->
    L.build_call array_retrieve_float_func [|(expr builder a) ; (expr builder b)|]
    "array_retrieve_float" builder
      | Call (f, act) ->
         let (fdef, fdecl) = try StringMap.find f function_decls with
                              Not_found -> raise (Failure "223")
          in
	 let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	 let result = (match fdecl.ftyp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in
    
    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "f builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder f =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
	Block sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | Expr e -> let _ = expr builder e in builder 
      | Return e -> let _ = match fdecl.ftyp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         (* Add "merge" basic block to our function's list of blocks *)
	 let merge_bb = L.append_block context "merge" the_function in
         (* Partial function used to generate branch to merge block *) 
         let branch_instr = L.build_br merge_bb in

         (* Same for "then" basic block *)
	 let then_bb = L.append_block context "then" the_function in
         (* Position builder in "then" block and build the statement *)
         let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
         (* Add a branch to the "then" block (to the merge block) 
           if a terminator doesn't already exist for the "then" block *)
	 let () = add_terminal then_builder branch_instr in

         (* Identical to stuff we did for "then" *)
	 let else_bb = L.append_block context "else" the_function in
         let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
	 let () = add_terminal else_builder branch_instr in

         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
	 let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (* Move to the merge block for further instruction building *)
	 L.builder_at_end context merge_bb

      | While (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
	  let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
	  let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
	  let body_bb = L.append_block context "while_body" the_function in
          let while_builder = stmt (L.builder_at_end context body_bb) body in
	  let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
	  let merge_bb = L.append_block context "merge" the_function in
	  let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | For (e1, e2, e3, body) -> stmt builder
	    ( Block [Expr e1 ; While (e2, Block [body ; Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (Block fdecl.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.ftyp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
