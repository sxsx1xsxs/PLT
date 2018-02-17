{ open Parser }

rule tokenize = parse
    [' ', '\t', '\n'] { tokenize lexbuf }
  | "/*"    { comment lexbuf }
  | '+'     {PLUS}
  | '-'     {MINUS}
  | '*'     {TIMES}
  | '/'     {DIVIDE}
  | '%'     {MOD}
  | '@'     {REGEX}
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
  | "true"  {TRUE}
  | "false" {FALSE}
  | "null"  {NULL}
  | '"' [^'"']* '"' as lit {STRING(lit)}
  | ['0'-'9']+ as num { LITERAL(int_of_string num) }
  | eof     {EOF}

  and comment = parse
    "*/" { tokenize lexbuf }
  | _    { comment lexbuf }

