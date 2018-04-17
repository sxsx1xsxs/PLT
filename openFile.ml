type action = Ast | Compile

let () =

    let usage_msg = "usage: ./openFile.native [file.of]" in
    let channel = ref stdin in
    Arg.parse [] (fun file -> channel := open_in file) usage_msg;
    let lexbuf = Lexing.from_channel !channel in
    let ast = Parser.program Scanner.next_token lexbuf in

	let m = Codegen.translate ast in
	Llvm_analysis.assert_valid_module m;

    print_string (Llvm.string_of_llmodule m);;
        (*print_string (Ast.string_of_program ast);;*)
