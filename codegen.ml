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

module StringMap = Map.Make(String)

  (* Helper function for assigning struct values. *)
  let build_struct_assign str values builder =
  let assign (llv, ind) value =
    match value with
    | Some v -> (L.build_insertvalue llv v ind "" builder, ind+1)
    | None -> (llv, ind+1)
  in
  let (ret, _) = Array.fold_left assign (str, 0) values in ret

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
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> str_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.Arr (typ, len) -> L.array_type (ltype_of_typ typ) len
  in

  let rec global_init_expr = function
      A.Literal i -> L.const_int i32_t i
    | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | A.Sliteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in 
    L.const_bitcast (L.const_gep l [| L.const_int i32_t 0|]) str_t
    | A.Fliteral f -> L.const_float float_t f
    | A.Noexpr -> L.const_int i32_t 0
	| A.Array_Lit l ->
      let lll = Array.of_list (List.map global_init_expr l) in
      let typ = L.type_of (Array.get lll 0) in
      L.const_array typ lll
    | _ -> raise (Failure ("not found"))
  in

  let global_init_noexpr = function
    | A.Int -> L.const_int i32_t 0
    | A.Bool -> L.const_int i1_t 0
    | A.String -> global_init_expr(A.Sliteral "")
    | A.Void -> L.const_int void_t 0
    | A.Float -> L.const_float float_t 0.0
    | A.Arr (typ, len)-> global_init_expr(A.Array_Lit [])
  in

  let local_init_expr = function
      A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Sliteral s -> let l = L.define_global "" (L.const_stringz context s) the_module in L.const_bitcast (L.const_gep l [| L.const_int i32_t 0|]) str_t
      | A.Fliteral f -> L.const_float float_t f
      | A.Noexpr -> L.const_int i32_t 0
      | A.Array_Lit l ->
        let lll = Array.of_list (List.map global_init_expr l) in
        let typ = L.type_of (Array.get lll 0) in
        L.const_array typ lll
      | _ -> raise (Failure ("not found"))
  in

  let local_init_noexpr = function
      | A.Int -> L.const_int i32_t 0
      | A.Bool -> L.const_int i1_t 0
      | A.String -> global_init_expr(A.Sliteral "")
      | A.Void -> L.const_int void_t 0
      | A.Float -> L.const_float float_t 0.0
      | A.Arr (typ, len) -> local_init_expr(A.Array_Lit [])
  in

  let printf_t = L.var_arg_function_type i32_t [| str_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in
  
  let strlen_t = L.function_type float_t [| str_t |] in
  let strlen_func = L.declare_function "strlen" strlen_t the_module in
  
  (* let create_t = L.function_type void_p [||] in
  let create_func = L.declare_function "create" create_t the_module in
  *)
  (*
  let array_add_string_t = L.function_type i32_t [|void_p ; str_t ; str_t|] in
  let array_add_string_func = L.declare_function "array_add_string" array_add_string_t the_module in

  let array_retrieve_string_t = L.function_type str_t [|void_p ; str_t|] in
  let array_retrieve_string_func = L.declare_function "array_retrieve_string" array_retrieve_string_t the_module in

  let array_add_float_t = L.function_type i32_t [|void_p ; str_t ; float_t|] in
  let array_add_float_func = L.declare_function "array_add_float" array_add_float_t the_module in

  let array_retrieve_float_t = L.function_type float_t [|void_p ; str_t|] in
  let array_retrieve_float_func = L.declare_function "array_retrieve_float" array_retrieve_float_t the_module in
  *)
  

  
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
	Array.of_list (List.map (fun (A.VarDecl(t, _, _)) -> ltype_of_typ t ) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.ftyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n g_map l_map = try StringMap.find n l_map
                   with Not_found -> 
                        try StringMap.find n g_map with
                          Not_found -> raise (Failure "Variable not found")
                      in
    (* let lookup_func n = fst (StringMap.find n function_decls) in *)

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = try StringMap.find fdecl.A.fname function_decls 
    with Not_found -> raise (Failure "Function not found")
    in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (*let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder*)
    let char_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    (* and string_format_str = L.build_global_stringptr "%s\n" "fmt2" builder *) 
    and float_format_str = L.build_global_stringptr "f\n" "fmt" builder
    in


    let build_string e builder = 
        let str = L.build_global_stringptr e "str" builder in
        let null = L.const_int i32_t 0 in
    L.build_in_bounds_gep str [| null |] "str" builder in

    (* This is where GM left at Fri. 8:05 PM *)

	
	(* Construct code for an expression used for assignment; return its value *)
    let rec lexpr builder g_map l_map = function
	  | A.Id s -> L.build_load (lookup s g_map l_map) s builder
	  | A.Assign (l, r) ->
        let l', r' = lexpr builder g_map l_map l, expr builder g_map l_map r in
        ignore (L.build_store r' l' builder); l'
	  | A.Array_Index (arr, ind) ->
        let arr' = L.build_load (lookup arr g_map l_map) arr builder in
        let ind' = expr builder g_map l_map ind in
        L.build_gep arr' [|L.const_null i32_t; ind'|] "Array_Index" builder
	  | _ -> raise (Failure ("not found"))(* Semant should catch other illegal attempts at assignment *)
		
    (* Construct code for an expression; return its value *)
    and expr builder g_map l_map = function
        A.Literal i -> L.const_int i32_t i
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Fliteral l -> L.const_float float_t l
      | A.Sliteral s -> build_string s builder
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s g_map l_map) s builder
      | A.Binop (e1, op, e2) -> 
	  (* let (t, _) = e1 *)
	  let e1' = expr builder g_map l_map e1
	  and e2' = expr builder g_map l_map e2 in
	  if (L.type_of e1' = float_t || L.type_of e2' = float_t) then
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
      else
      (match op with
        A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mult    -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Equal   -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Less    -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Greater -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
  
      | A.Unop(op, e) ->
	  let e' = expr builder g_map l_map e in
	  (match op with
	  | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder

      (* assume only float need semantic checking *)
	  (*
      | A.Array_Index (s, e) -> L.build_call array_retrieve_float_func [|(L.build_load (lookup s g_map l_map) s builder) ; (expr builder g_map l_map e)|] "array_retrieve" builder
      | A.Array_Assign (s, i, e) -> L.build_call array_add_float_func [|(L.build_load (lookup s g_map l_map) s builder) ; (expr builder g_map l_map i) ; (expr builder g_map l_map e)|]
            "array_add_float" builder
      | A.Assign (e1, e2) -> let l_val = (lookup e1 g_map l_map) in
                         let e2'  = (expr builder g_map l_map e2) in
						 ignore (L.build_store e2' l_val builder); e2'
	  *)
	  | A.Assign _ as e -> L.build_load (lexpr builder g_map l_map e) "" builder
	  | A.Array_Lit l ->
	    let lll = List.map (expr builder g_map l_map) l in
		let typ = (L.array_type (L.type_of (List.hd lll)) (List.length l)) in
		let lll = List.map (fun x -> Some x) lll in
		build_struct_assign (L.undef typ) (Array.of_list lll) builder
	  | A.Array_Index (arr, ind) ->
	    let arr', ind' = L.build_load (lookup arr g_map l_map) arr builder, expr builder g_map l_map ind in
		let arr_ptr = L.build_alloca (L.type_of arr') "arr" builder in
		ignore (L.build_store arr' arr_ptr builder);
		L.build_load (L.build_gep arr_ptr [|L.const_null i32_t; ind'|] "" builder) "Array_Index" builder
	  
      (*| Call ("prints", [_]) -> L.build_call prints_func [| str_format_str |] "prints" builder*)
      | A.Call ("prints", [e]) -> L.build_call printf_func [| char_format_str; (expr builder g_map l_map e)|] "printf" builder
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder g_map l_map e) |]
	    "printf" builder
      | A.Call ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder g_map l_map e) |] "printbig" builder
      | A.Call ("printf", [e]) -> 
	  L.build_call printf_func [| float_format_str ; (expr builder g_map l_map e) |]
	    "printf" builder
      | A.Call ("strlen", [e]) ->
    L.build_call strlen_func [|expr builder g_map l_map e|] 
    "strlen" builder
    (*  | A.Call ("array_add_string", [a;b;c]) ->
    L.build_call array_add_string_func [|(expr builder g_map l_map a) ; (expr builder g_map l_map b) ; (expr builder g_map l_map c)|] 
    "array_add_string" builder
      | A.Call ("array_retrieve_string", [a; b]) ->
    L.build_call array_retrieve_string_func [|(expr builder g_map l_map a) ; (expr builder g_map l_map b)|]
    "array_retrieve_string" builder
      | A.Call ("array_add_float", [a; b; c]) ->
    L.build_call array_add_float_func [|(expr builder g_map l_map a) ; (expr builder g_map l_map b) ; (expr builder g_map l_map c)|]
    "array_add_float" builder
      | A.Call ("array_retrieve_float", [a; b]) ->
    L.build_call array_retrieve_float_func [|(expr builder g_map l_map a) ; (expr builder g_map l_map b)|]
    "array_retrieve_float" builder *)
      | A.Call (f, act) ->
         let (fdef, fdecl) = try StringMap.find f function_decls with
                              Not_found -> raise (Failure "Founction not declared")
          in
	 let actuals = List.rev (List.map (expr builder g_map l_map) (List.rev act)) in
	 let result = (match fdecl.A.ftyp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in
	
    (* Declare each global variable; remember its value in a map *)
    let global_vars = 
      let global_var m (A.VarDecl(_, n, e)) =
        let init = global_init_expr e in
        StringMap.add n (L.define_global n init the_module) m in
      List.fold_left global_var StringMap.empty globals in
	
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (A.VarDecl(t, n, _)) p = 
	  	let () = L.set_value_name n p in
      let local = L.build_alloca (ltype_of_typ t) n builder in
      let _  = L.build_store p local builder in
	StringMap.add n local m 
    in
	
      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (A.VarDecl(t, n, e)) =
        let e' = match e with
            A.Noexpr -> local_init_noexpr t
            | _ -> local_init_expr e
        in
      L.set_value_name n e';
	let l_var = L.build_alloca (ltype_of_typ t) n builder in
    ignore (L.build_store e' l_var builder);
    StringMap.add n l_var m in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
    List.fold_left add_local formals fdecl.A.locals 
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
	    A.Block sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | A.Expr e -> let _ = expr builder global_vars local_vars e in builder 
      | A.Return e -> ignore (match fdecl.A.ftyp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder global_vars local_vars e) builder); builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder global_vars local_vars predicate in
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

      | A.While (predicate, body) ->
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
	  let bool_val = expr pred_builder global_vars local_vars predicate in

          (* Hook everything up *)
	  let merge_bb = L.append_block context "merge" the_function in
	  let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.ftyp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
