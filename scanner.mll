{ open Parser }

rule tokenize = parse
    [' ', '\t', '\n'] { tokenize lexbuf }
  | '+'     {PLUS}
  | '-'     {MINUS}
  | '*'     {TIMES}
  | '/'     {DIVIDE}
  | '%'     {MOD}
  | '@'     {REGEX}
  | '=='    {EQ}
  | '!='    {NEQ}
  | '>>'    {RAPPEND}
  | '<<'    {LAPPEND}
  | '>'     {LR}
  | '<'     {RL}
  | '<='    {LEQ}
  | '>='    {REQ}
  | 'and'   {AND}
  | 'or'    {OR}
  | 'not'   {NOT}
  | ';'     {SEMI}
  | ','     {COMMA}
  | '('     {LPR}
  | ')'     {RPR}
  | '['     {LBK}
  | ']'     {RBK}
  | '{'     {LBC}
  | '}'     {RBC}
  | 'if'    {IF}
  | 'elif'  {ELIF}
  | 'else'  {ELSE}
  | 'for'   {FOR}
  | 'while' {WHILE}
  | 'true'  {TRUE}
  | 'false' {FALSE}
  | 'null'  {NULL}
