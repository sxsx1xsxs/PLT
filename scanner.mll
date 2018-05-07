{ 
    open Parser
    type pattern = RE | NORMAL
    let state_ref = ref NORMAL
}

(* define some common character classes here *)
let digits = ['0' - '9']+

rule token pat = parse
    [' ' '\t' '\n'] { token pat lexbuf }
  | "/*"    { comment pat lexbuf }
  | "="     {ASSIGN}
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
  | "true"  { BOOL_T(true) }
  | "false" { BOOL_T(false) }
  | "void"  {VOID}
  | "float" {FLOAT}
  | "bool"  {BOOL}
  | "int"   {INT}
  | "string" {STRING}
  | "regex" {REGEXP}
  | "file" {FILE}
  | '"' [^'"']* '"' as lit { STRING_T(lit) }
  | '"' [^'"']* '"' as lit { FILE_T(lit) }
  | ['-']?['0' - '9']+['.']['0' - '9']+ as lxm {FLOAT_T(float_of_string lxm)}
  | ['-']?digits as num { INT_T(int_of_string num) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID (lxm) }
  | eof     {EOF}
  | _ as error { raise (Failure("illegal character " ^ Char.escaped error)) }

  and comment pat = parse
    "*/" { token pat lexbuf }
  | _    { comment pat lexbuf }
  
  and regex pat = parse
  | '@' { pat := NORMAL ; REGEX }
  | ['\\']['"' '.' '?' '|' '^' '+' '[' ']' '(' ')' '\\' '*' '@'] as lit { REGEX_STRING(lit) }
  | '.' { DOT  }
  | '^' { HAT  }
  | '?' { QUST }
  | '*' { KLEN }
  | '-' { RANG }
  | '+' { RPLS }
  | '|' { ALTR }
  | '[' { LBRK }
  | ']' { RBRK }
  | '(' { LPRT }
  | ')' { RPRT }
  | [^'"' '.' '@' '|' '^' '?' '+' '[' ']' '(' ')' '\\' '*'] as lit {REGEX_STRING((Char.escaped lit))}
  | _ as error {raise (Failure("illegal regex " ^ Char.escaped error))}

  {
    let next_token lexbuf = match !state_ref with
       | NORMAL -> token state_ref lexbuf
       | RE -> regex state_ref lexbuf
  }
