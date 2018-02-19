{ open Parser }

rule token pat = parse
    [' ', '\t', '\n'] { token pat lexbuf }
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
  | "null"  {NULL}
  | '"' [^'"']* '"' as lit {STRING(lit)}
  | ['0'-'9']+ as num { LITERAL(int_of_string num) }
  | eof     {EOF}

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
