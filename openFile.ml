type action = Ast | Compile

let () =
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-a", Ast);
									("-c", Compile)]
	else Compile in

    let usage_msg = "usage: ./openFile.native [file.of]" in
    let channel = ref stdin in
    Arg.parse [] (fun file -> channel := open_in file) usage_msg;
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.next_token lexbuf in

    match !action with
    Ast -> print_string (Ast.string_of_program ast)
    |Compile -> 
(*     	let sast = Semant.convert ast in *)
(*     	let _ = Check.check sast in  *)
	let m = Codegen.translate ast in
	Llvm_analysis.assert_valid_module m; 

    print_string (Llvm.string_of_llmodule m);;
