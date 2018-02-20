{ open Parser }

(* define some common character classes here *)
let digits = ['0' - '9']+

rule token pat = parse
    [' ' '\t' '\n'] { token pat lexbuf }
  | "/*"    { comment pat lexbuf }
  | '+'     {PLUS}
  | '-'     {MINUS}
  | '*'     {TIMES}
  | '/'     {DIVIDE}
  | '%'     {MOD}
  | '@'     {pat := RE; REGEX}
  | "=="    {EQ}
  | "!="    {NEQ}
  | ">>"    {RAPPEND}
  | "<<"    {LAPPEND}
  | '>'     {LR}
  | '<'     {RL}
  | "<="    {LEQ}
  | ">="    {REQ}
  | "and"   {AND}
  | "or"    {OR}
  | "not"   {NOT}
  | ';'     {SEMI}
  | ','     {COMMA}
  | '('     {LPR}
  | ')'     {RPR}
  | '['     {LBK}
  | ']'     {RBK}
  | '{'     {LBC}
  | '}'     {RBC}
  | "if"    {IF}
  | "elif"  {ELIF}
  | "else"  {ELSE}
  | "for"   {FOR}
  | "while" {WHILE}
  | "return" {RETURN}
  | "true"  {TRUE}
  | "false" {FALSE}
  | "void"  {VOID}
  | "float" {FLOAT}
  | "bool"  {BOOL}
  | "int"   {INT}
  | "string" {STRING}
  | '"' [^'"']* '"' as lit { STRING_T(lit) }
  | ['-']?digits as lxm {FLOAT_T(float_of_string lxm)}
  | ['-']?['0' - '9']+['.']['0' - '9']+ as lxm {FLOAT_T(float_of_string lxm)}
  | ['0'-'9']+ as num { INT_T(int_of_string num) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID (lxm) }
  | eof     {EOF}
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

  and comment pat = parse
    "*/" { tokenize lexbuf }
  | _    { comment lexbuf }

  and regex pat = parse
  | "@" { pat := NORMAL ; REGEX }
  | _ as lit {REGEX_STRING(lit)}

  {
    let next_token lexbuf = match !state_ref with
       | NORMAL -> token state_ref lexbuf
       | RE -> regex state_ref lexbuf
  }
